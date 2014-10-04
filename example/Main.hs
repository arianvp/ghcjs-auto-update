module Main where
import Control.Monad

import Control.AutoUpdate


main = do
  update <- mkAutoUpdate $ do
    putStrLn "hello, world"


  forever update
