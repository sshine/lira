-- MIT License
-- 
-- Copyright (c) 2019 eToroX Labs
-- 
-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
-- 
-- The above copyright notice and this permission notice shall be included in all
-- copies or substantial portions of the Software.
-- 
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
-- SOFTWARE.

module Main where

import           Lira.Backends.Evm.EvmCompiler      as EVMC
import           Lira.Backends.Evm.Abi
import           Lira.Backends.IntermediateCompiler as IMC
import           Lira.Parser                        as LP
import           Lira.TypeChecker                   as TC

import           Data.Aeson
import qualified Data.ByteString.Lazy          as BS
import           Data.List.Split (splitOn)
import           System.Environment (getArgs)
import           System.Exit (exitFailure)


-- (outdir, bn, fp)
args2fileInfo :: [String] -> (String, String, String)
args2fileInfo [fp] =
  let fPath = head $ splitOn ".bahr" $ head $ splitOn ".dag" fp
      bn    = last $ splitOn "/" fPath
  in  ("", bn, fp)
args2fileInfo ["-o", outdir, fp] =
  let fPath = head $ splitOn ".bahr" $ head $ splitOn ".dag" fp
      bn    = last $ splitOn "/" fPath
  in  (outdir, bn, fp)
args2fileInfo _ = ("", "", "")

-- We would like to call 'Main -o "$outdir" <file>'
main :: IO ()
main = do
  files <- getArgs
  let (outdir, bn, fp) = args2fileInfo files
  case bn of
    "" -> do
      putStrLn "Usage: Main [-o outdir] <file name>"
      exitFailure
    _ -> do
      let binPath = outdir ++ "/" ++ bn ++ ".bin"
      source <- readFile fp
      let parseRes = LP.parseWrap source
      case parseRes of
        Left err -> putStrLn
          ("Parse error! " ++ show err ++ "\n\nSource code:\n" ++ source)
        -- DEVFIX: The error handling could probably be better here
        -- Do we need checks after the parser is successful?
        Right ast -> do
          let typeCheck = TC.typeChecker ast
          case typeCheck of
            Left  errTC -> putStrLn ("Type check error! " ++ show errTC)
            Right astTC -> do
              putStrLn ("Writing to file " ++ binPath)
              writeAbiDef outdir bn
              writeFile binPath (assemble $ intermediateCompile astTC)
