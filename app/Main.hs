module Main where

import qualified Language.NonSense.Parser as NS
import qualified Language.NonSense.Transpiler as NS
import qualified Language.TypeScript.AST as TS
import qualified Language.TypeScript.PrettyPrinter as TS
import NSPrelude
import System.Environment (getArgs)
import System.FilePath (replaceExtensions)

builtin :: [TS.Declaration]
builtin = []

main :: IO ()
main =
  getArgs >>= \case
    [inputPath] -> do
      input <- readFile inputPath
      case NS.parseModule input of
        Right decls -> do
          let output = TS.pretty $ TS.Module $ builtin <> foldMap NS.transpileDeclaration decls
          let outputPath = replaceExtensions inputPath "ts"
          writeFile outputPath output
        Left err ->
          putStrLn err
    _ -> putStrLn "should be: ns file-path"
