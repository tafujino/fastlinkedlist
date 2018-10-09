module Main where

import Test.DocTest

main :: IO ()
main = doctest ["-ilib",
                "lib/Data/Offheapvector.hs",
                "lib/Data/ArrayLinkedList/DLList.hs"
               ]
