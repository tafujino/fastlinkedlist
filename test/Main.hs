module Main where

import Test.DocTest

main :: IO ()
main = doctest ["lib/Data/ArrayLinkedList/SLList.hs",
                "lib/Data/ArrayLinkedList/DLList.hs"]
