module Main where

import           App.Settings                             ( options )
import           App                                      ( runApp )

main :: IO ()
main = options >>= runApp
