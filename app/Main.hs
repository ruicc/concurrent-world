{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Cont
import Control.Applicative
import Control.Concurrent
import qualified Control.Exception as E
import Control.Lens
import Control.Lens.TH
import Data.Unique
import Text.Read (readMaybe)

import System.IO
import System.Environment (getArgs)
import Network

import Lib

main :: IO ()
main = do
    print =<< bracket_demo

    port:_ <- map read <$> getArgs
    makeWorld port

bracket_demo = (`runContT` return) $ do
    fh <- ContT $ E.bracket
            (openFile "tmp" AppendMode)
            hClose
    n <- ContT $ E.bracket
            (hPutStrLn fh "Gain n" >> return 42)
            (\n -> hPutStrLn fh $ "Finalize n: " ++ show n)
    l <- ContT $ E.bracket
            (hPutStrLn fh "Gain l" >> return 13)
            (\n -> hPutStrLn fh $ "Finalize l: " ++ show n)
--    liftIO $ E.throwIO (E.ErrorCall "heyhey")
    return $ n + l

-- sketch
makeWorld :: Port -> IO ()
makeWorld port = do
    acceptLoop port $ \ client -> (`runContT` return) $ do
        logined <- ContT $ login client
        roomId <- ContT $ joinRoom logined
        ContT $ chat logined roomId

