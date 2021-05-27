{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Data.List.NonEmpty (NonEmpty)
import Numeric.Natural (Natural)

import qualified Algebra.Graph.Export.Dot
import qualified Data.List.NonEmpty as NonEmpty
import qualified Nix.Graph
import qualified Options.Applicative as Options

data Options = Options
  { paths :: NonEmpty FilePath
  , excludeBuilt :: Bool
  , excludeCached :: Bool
  , maxFiles :: Natural
  }

parseOptions :: Options.Parser Options
parseOptions = do
  paths <-
    fmap NonEmpty.fromList $
      Options.some $ Options.strArgument (Options.metavar "PATH")

  excludeBuilt <-
    Options.switch . mconcat $
      [ Options.long "exclude-built"
      , Options.help . mconcat $
          [ "Reduce size of graph by excluding derivations which have already"
          , "been built, and have their output path in the local Nix store."
          ]
      , Options.hidden
      ]

  excludeCached <-
    Options.switch . mconcat $
      [ Options.long "exclude-cached"
      , Options.help . mconcat $
          [ "Reduce size of graph by excluding derivations which do not need to"
          , "be built (because they have already been built locally) or can be"
          , "substituted from another cache. Implies `--exclude-built`."
          ]
      , Options.hidden
      ]

  maxFiles <-
    Options.option Options.auto . mconcat $
      [ Options.long "max-files"
      , Options.metavar "COUNT"
      , Options.help "Limit number of open files"
      , Options.value 100
      , Options.showDefault
      , Options.hidden
      ]

  pure Options{paths, excludeBuilt, excludeCached, maxFiles}

getOptions :: IO Options
getOptions = do
  let parserPrefs =
        Options.prefs . mconcat $
          [ Options.showHelpOnError
          , Options.multiSuffix ".."
          ]
  let parserInfo = Options.info (Options.helper <*> parseOptions) mempty
  Options.customExecParser parserPrefs parserInfo

main :: IO ()
main = do
  Options{paths, excludeBuilt, excludeCached, maxFiles} <- getOptions

  let exclude
        | excludeCached = Nix.Graph.ExcludeCached
        | excludeBuilt = Nix.Graph.ExcludeBuilt
        | otherwise = Nix.Graph.ExcludeNothing

  let config =
        Nix.Graph.Config
          { Nix.Graph.exclude
          , Nix.Graph.maxFiles
          }

  adjacencyMap <- Nix.Graph.build config (NonEmpty.toList paths)

  putStrLn (Algebra.Graph.Export.Dot.exportAsIs adjacencyMap)
