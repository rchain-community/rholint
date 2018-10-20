--
-- Copyright 2018 Includable.
-- Licensed under MIT license.
--

{-# LANGUAGE DeriveGeneric #-}

module Main where

import System.IO ( stdin, hGetContents )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )
import Control.Monad (when)
import Data.List (intercalate)
import Data.Aeson (ToJSON, encode, toEncoding, genericToEncoding, defaultOptions)
import GHC.Generics
import qualified Data.ByteString.Lazy.Char8 as B

import LexRholang
import ParRholang
import SkelRholang
import PrintRholang
import AbsRholang

import ErrM

type ParseFun a = [Token] -> Err a

type Verbosity = Int

data LexerError = LexerError {
  kind     :: String,
  severity :: Int,
  message  :: String
} deriving (Generic, Show)

data LexerResult = LexerResult {
  success  :: Bool,
  err      :: [LexerError],
  result   :: String,
  tree     :: String,
  parsed   :: String
} deriving (Generic, Show)

instance ToJSON LexerError where
  toEncoding = genericToEncoding defaultOptions
instance ToJSON LexerResult where
  toEncoding = genericToEncoding defaultOptions

printConcat :: Show a => [a] -> IO ()
printConcat = putStrLn . (intercalate "," . map show)

runFile :: (Print a, Show a) => Verbosity -> ParseFun a -> FilePath -> IO ()
runFile v p f = putStrLn f >> readFile f >>= run v p

run :: (Print a, Show a) => Verbosity -> ParseFun a -> String -> IO ()
run v p s = let ts = myLexer s in case p ts of
           Bad s    -> do B.putStrLn (encode (LexerResult {
                            success = False,
                            err     = [LexerError {
                              kind     = "syntax",
                              severity = 1,
                              message  = s
                            }],
                            result  = show ts,
                            tree    = "",
                            parsed  = ""
                          }))
                          exitFailure
           Ok  tree -> do B.putStrLn (encode (LexerResult {
                            success = True,
                            err     = [],
                            result  = show ts,
                            tree    = show tree,
                            parsed  = printTree tree
                          }))
                          exitSuccess

usage :: IO ()
usage = do
  putStrLn $ unlines
    [
      "usage: rholint [...files]",
      "",
      "  --help          Display this help message",
      "  (files)         Parse content of files",
      "  (no arguments)  Parse stdin"
    ]
  exitFailure

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    [] -> getContents >>= run 2 pContr
    fs -> mapM_ (runFile 2 pContr) fs





