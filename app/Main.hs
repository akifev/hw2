module Main where

import Lib
import Report
import System.Directory
import Data.Maybe

import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  helloMessage
  startFileManager

helloMessage :: IO ()
helloMessage = do
  putStrLn "┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓"
  putStrLn "┃ Command Line File Manager written in Haskell ┃"
  putStrLn "┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛"

startSessionMessage :: IO ()
startSessionMessage = putStrLn "> Start the session..."

startFileManager :: IO ()
startFileManager = do
  putStr "> Enter a path to working directory >>> /"
  line <- getLine
  let path = cdHard "/" line
  isExistingDirectory <- doesDirectoryExist path
  if isExistingDirectory
  then do
    startSessionMessage
    run path
  else do
    reportIncorrectPath "It'd be path to existing directory. Try again."
    startFileManager