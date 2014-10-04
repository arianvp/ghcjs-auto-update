module Control.AutoUpdate.Util
( requestAnimationFrame
, cancelAnimationFrame
, waitForAnimationFrame
, handle
, cb
)

where

import GHCJS.Types
import GHCJS.Foreign
import Control.Concurrent.MVar (putMVar, takeMVar, newEmptyMVar)
import System.IO hiding (Handle)

newtype Handle = Handle (JSRef Handle)

foreign import javascript unsafe "requestAnimationFrame"
  js_requestAnimationFrame :: JSFun (IO ()) -> IO Handle


foreign import javascript unsafe "cancelAnimationFrame($1)"
  js_cancelAnimationFrame :: Handle -> IO ()


data AnimationFrameHandle  = AnimationFrameHandle { handle :: Handle, cb :: JSFun (IO ())}

requestAnimationFrame :: IO () -> IO AnimationFrameHandle
requestAnimationFrame x = do
    cb <- fixIO $ \cb -> syncCallback AlwaysRetain True (release cb >> x)
    h <- js_requestAnimationFrame cb
    return $ AnimationFrameHandle h cb

cancelAnimationFrame :: AnimationFrameHandle -> IO ()
cancelAnimationFrame (AnimationFrameHandle h cb) =
  release cb >> js_cancelAnimationFrame h



waitForAnimationFrame :: IO ()
waitForAnimationFrame = do
  v <- newEmptyMVar
  requestAnimationFrame $ putMVar v ()
  takeMVar v

