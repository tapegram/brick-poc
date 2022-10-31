module UI (main) where

import Brick

ui :: Widget ()
ui = str "Hello, world!"

main :: IO ()
main = simpleMain ui