module Main (main) where

import Main.Utf8 qualified
import Podenv.Main qualified

main :: IO ()
main = Main.Utf8.withUtf8 Podenv.Main.main
