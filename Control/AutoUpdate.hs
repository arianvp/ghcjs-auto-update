module Control.AutoUpdate
( mkAutoUpdate
) where

import          Control.Monad
import          Control.Concurrent            (forkIO)
import          Control.Concurrent.MVar       (newEmptyMVar, putMVar, readMVar,
                                               takeMVar, tryPutMVar, tryTakeMVar)
import          Control.Exception             (SomeException, catch, throw)
import          Data.IORef                    (newIORef, readIORef, writeIORef)

import          Control.AutoUpdate.Util       (waitForAnimationFrame)

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
    
    a <- catchSome ua
    
    writeIORef currRef $ Just a
    void $ tryTakeMVar lastValue
    putMVar lastValue a

    -- delay until we're needed again
    waitForAnimationFrame

    -- delay's over, clear out currRef andd lastValue so that demanding the
    -- value again forces us to start work
    writeIORef currRef Nothing
    void $ takeMVar lastValue
  
  return $ do
    mval <- readIORef currRef
    case mval of
      Just val -> return val
      Nothing  -> do
        void $ tryPutMVar needsRunning ()
        readMVar lastValue


-- | Turn a runtime exception into an impure exception, so that all @IO@
-- actions will complete successfully. This simply defers the exception until
--  the value is forced.
catchSome :: IO a -> IO a
catchSome act = Control.Exception.catch act $ \e -> return $ throw (e :: SomeException)

