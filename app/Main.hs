{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Cont
import Control.Applicative
import Control.Concurrent
import Control.Exception as E hiding (handle)
import Data.Unique
import Text.Read (readMaybe)

import System.IO
import System.Environment (getArgs)

import Lib

main :: IO ()
main = do
    print =<< bracket_demo

    port:_ <- map read <$> getArgs
    launchServer port

bracket_demo = (`runContT` return) $ do
    fh <- ContT $ bracket
            (openFile "tmp" AppendMode)
            hClose
    n <- ContT $ bracket
            (hPutStrLn fh "Gain n" >> return 42)
            (\n -> hPutStrLn fh $ "Finalize n: " ++ show n)
    l <- ContT $ bracket
            (hPutStrLn fh "Gain l" >> return 13)
            (\n -> hPutStrLn fh $ "Finalize l: " ++ show n)
--    liftIO $ throwIO (ErrorCall "heyhey")
    return $ n + l

-- sketch
launchServer :: Port -> IO ()
launchServer port = (`runContT` return) $ do
    client <- ContT $ acceptLoop port
    loginedClient <- ContT $ login client
    roomId <- ContT $ joinRoom loginedClient
    ContT $ chat loginedClient roomId


---- NPC
--data NPC = NPC { _npcId :: Int }
--
---- NPC spawner
--launchNpcServer :: Port -> IO ()
--launchNpcServer port (`runContT` return) $ do
--    npc <- ContT $ genNpcLoop port
--    loop npc
--  where
--    loop npc = forever $ do
--        mUsers <- ContT $ searchUser npc
--        case mUsers of
--            Nothing -> return ()
--            Just users -> forM_ users $ \user -> do
--                mChan <- ContT $ npc `tryConnect` user
--                case mChan of
--                    Nothing -> return ()
--                    Just chan = do
--                        ContT $ greet chan npc "Heyhey!"
--                        mResponse <- ContT $ wait chan
--                        case mResponse of
--                            Nothing -> return ()
--                            Just response -> do
--                                ContT $ greet chan "Oh thanks for your response, human living. Take care!"
--                                kill npc
