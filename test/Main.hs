module Main where

import Test.DocTest

main :: IO ()
main = doctest ["lib/Data/Offheapvector.hs",
                "lib/Data/ArrayLinkedList/SLList.hs",
                "lib/Data/ArrayLinkedList/DLList.hs"]
