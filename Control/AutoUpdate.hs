module Control.AutoUpdate
( mkAutoUpdate
) where

import          Control.Monad
import          Control.Concurrent            (forkIO)
import          Control.Concurrent.MVar       (newEmptyMVar, putMVar, readMVar,
                                               takeMVar, tryPutMVar, tryTakeMVar)
import          Control.Exception             (SomeException, catch, throw)
import          Data.IORef                    (newIORef, readIORef, writeIORef)

import          Control.AutoUpdate.Util       (cb, handle ,waitForAnimationFrame, requestAnimationFrame, cancelAnimationFrame)

import          GHCJS.Foreign
import          System.IO

mkAutoUpdate :: IO a -> IO (IO a)
mkAutoUpdate ua = do
  currRef <- newIORef Nothing
  needsRunning <- newEmptyMVar

  -- The last value generated, to allow for blocking semantics when currRef
  -- is Nothing.
  lastValue <- newEmptyMVar

  void $ forkIO $ forever $ do
    -- block until a value is actually needed.
    takeMVar needsRunning

    h <- fixIO $ \h -> requestAnimationFrame $ do
      a <- catch ua $ \e -> do
       cancelAnimationFrame h
       return $ throw (e :: SomeException)
      writeIORef currRef $ Just a
      void $ tryTakeMVar lastValue
      putMVar lastValue a
      writeIORef currRef Nothing
    void $ takeMVar lastValue
    release $ cb h

  return $ do
    mval <- readIORef currRef
    case mval of
      Just val -> return val
      Nothing  -> do
        void $ tryPutMVar needsRunning ()
        readMVar lastValue

