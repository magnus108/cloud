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
	  (pongPort,getPing) <- newChan 
          p <- spawn nid ($(mkClosure 'pingServer) pongPort) --start process der svarer her
	  return (p, getPing)

  forM_ ps $ \(pid, getPing)-> do                              -- <3>
    	say $ printf "pinging %s" (show pid)
 	pingPort <- receiveChan getPing -- fa svar fra process med hvor man skal spørge med ping
        (sendPong, recvPong) <- newChan :: Process (SendPort (), ReceivePort ())-- lav kanal til at sende ping 
	sendChan pingPort sendPong  -- send ping til process med hvor maan skal svare med pong
    	say $ printf "got pong" 
        receiveChan recvPong -- få svar med pong

  say "All pongs successfully received"
  terminate
-- >>

-- <<main
main :: IO ()
main = distribMain master __remoteTable
-- >>
