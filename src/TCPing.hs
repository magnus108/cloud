{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, DeriveGeneric #-}
{-# OPTIONS_GHC -Wall #-}
module TCPing
    ( main
    ) where
import Control.Distributed.Process hiding (Message)
import Control.Distributed.Process.Closure

import Control.Monad
import Text.Printf
import GHC.Generics (Generic)
import Data.Binary
import Data.Typeable

import DistribUtils

-- <<Message
type PingPort = SendPort (SendPort ())
type PongPort = SendPort PingPort

-- >>

-- <<pingServer
pingServer :: PongPort -> Process ()
pingServer pongPort = do
  (pingPort, getPong) <- newChan 
  sendChan pongPort pingPort
  say $ printf "expecting on ping" 
  x <- receiveChan getPong
  sendChan x ()
  return ()
-- >>

-- <<remotable
remotable ['pingServer]
-- >>

-- <<master
master :: [NodeId] -> Process ()
master peers = do

  ps <- forM peers $ \nid -> do
	  (givePingPort, getPingPort) <- newChan ---- lav kanal der kan smides ping port på
          p <- spawn nid ($(mkClosure 'pingServer) givePingPort) -- giv kanal der kan smides ping port på
	  return (p, getPingPort)

  forM_ ps $ \(pid, getPingPort)-> do          
    	say $ printf "pinging %s" (show pid)
 	pingPort <- receiveChan getPingPort -- få ping port
        (sendPongPort, getPong) <- newChan -- lav kanal der kan sendes pong på
	sendChan pingPort sendPongPort -- send ping med kanal der kan pongew på
    	say $ printf "got pong" 
        receiveChan getPong :: Process ()-- få pong

  say "All pongs successfully received"
  terminate
-- >>

-- <<main
main :: IO ()
main = distribMain master __remoteTable
-- >>
