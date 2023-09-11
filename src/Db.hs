{-# LANGUAGE OverloadedStrings #-}
module Db
    ( main
    ) where

import Control.Distributed.Process
import Control.Monad.IO.Class
import Control.Monad
import System.IO

import DistribUtils

import Database  (Database, createDB, get, set, rcdata)

master :: [NodeId] -> Process ()
master peers = do
  db <- createDB peers
  set db "gg" "hey"
  forever $ do
    l <- liftIO $ do putStr "key: "; hFlush stdout; getLine
    when (not (null l)) $ do
      r <- get db l
      liftIO $ putStrLn ("response: " ++ show r)

  return ()

main :: IO ()
main = distribMain master rcdata 
