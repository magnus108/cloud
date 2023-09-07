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
data Message = Ping (SendPort ProcessId)
  deriving (Typeable, Generic)

instance Binary Message
-- >>

-- <<pingServer
pingServer :: SendPort ProcessId -> Process ()
pingServer s = do
  mypid <- getSelfPid
  sendChan s mypid
-- >>

-- <<remotable
remotable ['pingServer]
-- >>

-- <<master
master :: [NodeId] -> Process ()
master peers = do

  ps <- forM peers $ \nid -> do
          say $ printf "spawning on %s" (show nid)
	  (s,r) <- newChan
          spawn nid ($(mkClosure 'pingServer) s)

	  ping <- receiveChan r

          (sendPong, recvPong) <- newChan
	  sendChan ping (Ping sendPong)
          receiveChan recvPong

  say "All pongs successfully received"
  terminate
-- >>

-- <<main
main :: IO ()
main = distribMain master __remoteTable
-- >>
