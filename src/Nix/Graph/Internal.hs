{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Nix.Graph.Internal where

import Algebra.Graph.AdjacencyMap (AdjacencyMap)
import Control.Applicative ((<|>))
import Control.Concurrent.STM.TSem (TSem)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Attoparsec.Text ((<?>))
import Data.Hashable (Hashable)
import Data.Set (Set)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import System.Exit (ExitCode (..))

import qualified Algebra.Graph.AdjacencyMap as AdjacencyMap
import qualified Control.Concurrent.STM.Map as STM.Map
import qualified Control.Concurrent.STM.TSem as TSem
import qualified Data.Attoparsec.Text as Attoparsec
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Nix.Derivation
import qualified UnliftIO.Async as Async
import qualified UnliftIO.Directory as Directory
import qualified UnliftIO.Exception as Exception
import qualified UnliftIO.IO as IO
import qualified UnliftIO.Process as Process
import qualified UnliftIO.STM as STM

-- Support `MonadFail` on GHC 8.6.5
#if MIN_VERSION_base(4,9,0)
import Control.Monad.Fail (MonadFail)
#endif
#if MIN_VERSION_base(4,13,0)
import Prelude hiding (MonadFail)
#endif

data Derivation = Derivation
  { derivationPath :: FilePath
  , derivationInputDrvs :: [FilePath]
  , derivationBuilt :: Bool
  }
  deriving stock (Eq, Ord, Generic)
  deriving anyclass (Hashable)

readDerivation :: (MonadIO m, MonadFail m) => TSem -> FilePath -> m Derivation
readDerivation tSem derivationPath = do
  let acquire = STM.atomically $ TSem.waitTSem tSem
  let release = STM.atomically $ TSem.signalTSem tSem

  drv <- liftIO . Exception.bracket_ acquire release $
    IO.withFile derivationPath IO.ReadMode $ \handle -> do
      fileContents <- Text.IO.hGetContents handle
      case Attoparsec.parseOnly Nix.Derivation.parseDerivation fileContents of
        Left err -> fail ("Failed to parse derivation: " <> err)
        Right drv -> pure drv

  outputPath <-
    case Map.lookup "out" (Nix.Derivation.outputs drv) of
      Nothing -> fail "Failed to lookup output path"
      Just output -> pure (Nix.Derivation.path output)

  derivationBuilt <- Directory.doesPathExist outputPath

  let derivationInputDrvs = Map.keys (Nix.Derivation.inputDrvs drv)

  pure Derivation{derivationPath, derivationBuilt, derivationInputDrvs}

buildAdjacencyMap ::
  MonadIO m =>
  Eq k =>
  Hashable k =>
  (k -> IO (Set k)) ->
  [k] ->
  m [(k, Set k)]
buildAdjacencyMap getNeighbors roots = liftIO $ do
  stmMap <- STM.atomically STM.Map.empty

  let go key = do
        isMember <- STM.atomically $ do
          isMember <- STM.Map.member key stmMap
          unless isMember $ STM.Map.insert key Set.empty stmMap
          pure isMember
        unless isMember $ do
          neighbors <- getNeighbors key
          STM.atomically $ STM.Map.insert key neighbors stmMap
          Async.mapConcurrently_ go neighbors

  Async.mapConcurrently_ go roots

  STM.Map.unsafeToList stmMap

filterUnbuilt :: (MonadIO m, MonadFail m) => [FilePath] -> m (Set FilePath)
filterUnbuilt derivationPaths = do
  (exitCode, _nixStdOut, nixStdErr) <-
    Process.readProcessWithExitCode
      "nix-store"
      ( [ "--realize"
        , "--dry-run"
        ]
          <> derivationPaths
      )
      ""

  when (exitCode /= ExitSuccess) $ do
    fail ("Failed to run 'nix-store --realize --dry-run " <> unwords derivationPaths <> "':\n" <> nixStdErr)

  case Attoparsec.parseOnly willBeBuilt (Text.pack nixStdErr) of
    Left parseError ->
      fail ("Failed to parse output from 'nix-store --realize --dry-run ...':\n" <> parseError)
    Right derivationPathsToBuild -> do
      pure (Set.fromList derivationPathsToBuild)
  where
    willBeBuilt :: Attoparsec.Parser [FilePath]
    willBeBuilt = Attoparsec.option [] $ do
      willBeBuiltHeading
      Attoparsec.many' nixStorePath

    -- Slightly different headings depending on Nix version
    --
    -- Nix 2: https://github.com/NixOS/nix/blob/2.3.10/src/libmain/shared.cc#L45-L71
    -- (uses the same strings from Nix 2.0 to 2.3.10)

    --
    -- Nix 3: https://github.com/NixOS/nix/blob/8e758d4/src/libmain/shared.cc#L48-L86
    -- (latest as of 2020-02-25)

    willBeBuiltHeading :: Attoparsec.Parser ()
    willBeBuiltHeading = do
      let nix2 = "these derivations will be built:"
      let nix3 =
            "this derivation will be built:"
              <|> ("these " *> Attoparsec.decimal @Int *> " derivations will be built:")
      _ <- nix2 <|> nix3
      Attoparsec.endOfLine

    nixStorePath :: Attoparsec.Parser FilePath
    nixStorePath = (<?> "/nix/store path") $ do
      _ <- "  "
      nixStore <- "/nix/store"
      rest <- Attoparsec.takeTill Attoparsec.isEndOfLine
      Attoparsec.endOfLine
      pure (Text.unpack (nixStore <> rest))

data Config = Config
  { exclude :: Exclude
  , maxFiles :: Natural
  }

data Exclude
  = ExcludeNothing
  | ExcludeBuilt
  | ExcludeCached
  deriving stock (Eq)

-- | Build graph of dependencies
build ::
  MonadIO m =>
  -- | Configure how the graph is built
  Config ->
  -- | Derivations to build graph from
  [FilePath] ->
  m (AdjacencyMap FilePath)
build _ [] = pure (AdjacencyMap.empty)
build Config{exclude, maxFiles} roots = liftIO $ do
  tSem <- STM.atomically $ TSem.newTSem (toInteger maxFiles)

  process :: [FilePath] -> IO [Derivation] <- do
    case exclude of
      ExcludeCached -> do
        unbuiltSet <- filterUnbuilt roots

        pure \paths -> do
          let unbuiltPaths = filter (`Set.member` unbuiltSet) paths

          Async.mapConcurrently (readDerivation tSem) unbuiltPaths
      ExcludeBuilt -> do
        pure \paths -> do
          derivations <- Async.mapConcurrently (readDerivation tSem) paths

          pure (filter (not . derivationBuilt) derivations)
      ExcludeNothing -> do
        pure \paths -> do
          Async.mapConcurrently (readDerivation tSem) paths

  let getInputDrvs :: Derivation -> IO (Set Derivation)
      getInputDrvs derivation = do
        fmap Set.fromList (process (derivationInputDrvs derivation))

  rootDrvs <- process roots

  adjacencySets <- buildAdjacencyMap getInputDrvs rootDrvs

  let adjacencyMap = AdjacencyMap.fromAdjacencySets adjacencySets

  pure (AdjacencyMap.gmap derivationPath adjacencyMap)
