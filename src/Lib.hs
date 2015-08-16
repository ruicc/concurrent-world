{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Lib where

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Cont
import Control.Applicative
import Control.Concurrent
import Control.Exception as E hiding (handle)
import Control.Lens
import Control.Lens.TH
import Data.Unique
import Text.Read (readMaybe)

import System.IO
import Network

type Port = Int
type RoomId = Int

data Client = Client
    { _clientHandle :: Handle
    }
data LoginedClient = LoginedClient
    { _loginedClientHandle :: Handle
    , _loginedClientId :: Int
    , _loginedClientName :: String
    }
makeFields ''Client
makeFields ''LoginedClient


acceptLoop :: Port -> (Client -> IO ()) -> IO ()
acceptLoop port cont = (`runContT` return) $ do
    let portId = PortNumber (fromIntegral port)
    socket <- ContT $ bracket
                            (listenOn portId)
                            (\socket -> putStrLn "...Socket closed" >> sClose socket)

    liftIO $ forever $ do
        (hdl, hostname, portNumber) <- accept socket
        let client = Client hdl
        forkFinally (cont client) (\_ -> hClose hdl)

login :: Client -> (LoginedClient -> IO r) -> IO r
login client cont = do
    let
        login' = do
            hPutStrLn (client ^. handle) "Name?"
            name <- hGetLine $ client ^. handle
            userId <- hashUnique <$> newUnique
            return $ LoginedClient (client^.handle) userId name
        logout = \cl -> return ()

    bracket login' logout cont

joinRoom :: LoginedClient -> (RoomId -> IO r) -> IO r
joinRoom loginedClient cont = do
    let
        hdl = loginedClient ^. handle
        loop = do
            hPutStrLn hdl "Room? [1-3]"
            inp <- hGetLine hdl
            case readMaybe inp of
                Just roomId
                    | 0 <= roomId && roomId <= 3 -> return roomId
                    | otherwise -> loop
                Nothing -> loop
        leave = \roomId -> return ()

    roomId <- loop
    
    bracket (return roomId) leave cont

chat :: LoginedClient -> RoomId -> (() -> IO r) -> IO r
chat = echo

echo :: LoginedClient -> RoomId -> (() -> IO r) -> IO r
echo cli roomId cont = go
  where
    go = do
        hPutStr (cli^.handle) "echo>"
        inp <- hGetLine (cli^.handle)
        if take 4 inp == "quit"
            then cont ()
            else do
                hPutStrLn (cli^.handle) inp
                go
