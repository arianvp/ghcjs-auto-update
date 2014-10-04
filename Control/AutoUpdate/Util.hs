module Control.AutoUpdate.Util
( requestAnimationFrame
, cancelAnimationFrame
, waitForAnimationFrame
)

where

import GHCJS.Types
import GHCJS.Foreign
import Control.Concurrent.MVar (putMVar, takeMVar, newEmptyMVar)
import System.IO hiding (Handle)

newtype Handle = Handle (JSRef Handle)

foreign import javascript unsafe "raf"
  js_requestAnimationFrame :: JSFun (IO ()) -> IO Handle


foreign import javascript unsafe "raf.cancel($1)"
  js_cancelAnimationFrame :: Handle -> IO ()


newtype AnimationFrameHandle  = AnimationFrameHandle (Handle, JSFun (IO ()))

requestAnimationFrame :: IO () -> IO AnimationFrameHandle
requestAnimationFrame x = do
    cb <- fixIO $ \cb -> syncCallback AlwaysRetain True (release cb >> x)
    h <- js_requestAnimationFrame cb
    return $ AnimationFrameHandle (h, cb)

cancelAnimationFrame :: AnimationFrameHandle -> IO ()
cancelAnimationFrame (AnimationFrameHandle (h, cb)) =
  release cb >> js_cancelAnimationFrame h



waitForAnimationFrame :: IO ()
waitForAnimationFrame = do
  v <- newEmptyMVar
  requestAnimationFrame $ putMVar v ()
  takeMVar v
