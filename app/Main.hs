module Main where

import qualified Language.NonSense.AST as NS
import qualified Language.NonSense.Parser as NS
import qualified Language.NonSense.Transpiler as NS
import qualified Language.NonSense.TypeChecker as NS
import qualified Language.TypeScript.AST as TS
import qualified Language.TypeScript.PrettyPrinter as TS
import NSPrelude
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath (replaceExtensions)

parseAndCheck :: FilePath -> IO [NS.Declaration]
parseAndCheck inputPath = do
  input <- readFile inputPath
  case NS.parseModule input of
    Right decls -> do
      case NS.check decls of
        Nothing -> pure decls
        Just (env, err) -> do
          putStrLn (NS.printError err)
          putStrLn (NS.printEnv env)
          exitFailure
    Left err -> do
      putStrLn err
      exitFailure

main :: IO ()
main =
  getArgs >>= \case
    ["transpile", inputPath] -> do
      decls <- parseAndCheck inputPath
      let output = TS.pretty (NS.transpileModule decls)
      let outputPath = replaceExtensions inputPath "ts"
      writeFile outputPath output
    ["typecheck", inputPath] -> do
      parseAndCheck inputPath
      putStrLn "No type errors"
    _ ->
      putStrLn $
        unlines
          [ "ns-exe transpile {path}",
            "ns-exe typecheck {path}",
            "ns-exe help"
          ]
