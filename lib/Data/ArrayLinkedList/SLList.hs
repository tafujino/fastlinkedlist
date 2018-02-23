{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# OPTIONS_GHC -fplugin=Foreign.Storable.Generic.Plugin #-}
{-# OPTIONS_GHC -fplugin-opt=Foreign.Storable.Generic.Plugin:-v1 #-}

module Data.ArrayLinkedList.SLList
  (
    SLList(),
    new,
    Iterator(),
    getBeginItr,
    getThisIx,
    getNextItr,
    insert,
    delete,
    forM_,
    forItrM_,
    (***)
  )
where

{-
Module      : Data.ArrayLinkedList.SLList
Description : A fast singly linked list implemented with an unboxed array

A singly linked list implemented using an unboxed array.
An array is expanded to the double size when it overflows.
A dummy node is introduced for speed.
-}

-- to implement foldlM, foldlM_

import qualified Data.OffHeapVector as OV
import qualified Data.FastStack as FS
import Foreign.Storable
import Foreign.Storable.Generic
import GHC.Generics
import Data.Default
import Data.IORef
import Data.Monoid

sentinelIx :: Int
sentinelIx = 0 

type CellIndex = Int -- index 0 refers to a sentinel
type CellSize  = Int

-- a cell in an array
data Cell a = Cell {
  getNextIx :: !CellIndex,
  getValue  :: !a
  } deriving (Show, Generic, GStorable, Default)

data SLList a = SLList {
  getArray   :: !(OV.OffHeapVector (Cell a)),
  getStack   :: !(FS.FastStack CellIndex)
  }

data Iterator a = Iterator {
  getList   :: !(SLList a),
  getPrevIx :: !CellIndex
  }

new :: (Default a, GStorable a) => CellSize -> IO (SLList a)
new initialCapacity = do
  array <- OV.new (initialCapacity + 1) -- one cell is additionaly reserved for a sentinel
  stack <- FS.new initialCapacity
  OV.pushBack array Cell { getNextIx = sentinelIx, getValue = def }
  return $ SLList array stack

getBeginItr :: (Default a, GStorable a) => SLList a -> Iterator a
getBeginItr list = Iterator { getList = list, getPrevIx = sentinelIx }

(***) :: (Default a, GStorable a) => Iterator a -> IO a
(***) itr = do
  ix <- getThisIx itr
  let list = getList itr
      array = getArray list
  thisCell <- OV.unsafeRead array ix
  return $ getValue thisCell

getThisIx :: (Default a, GStorable a) => Iterator a -> IO CellIndex
getThisIx itr = do
  let list = getList itr
      prevIx = getPrevIx itr
      array = getArray list
  prevCell <- OV.unsafeRead array prevIx
  return $ getNextIx prevCell  
  

isEnd :: (Default a, GStorable a) => Iterator a -> IO Bool
isEnd itr = do
  thisIx <- getThisIx itr
  return $ thisIx == sentinelIx

getNextItr :: (Default a, GStorable a) => Iterator a -> IO (Iterator a)
getNextItr itr = do
  thisIx <- getThisIx itr
  return $ Iterator { getList = getList itr, getPrevIx = thisIx }

-- |obtain an index of a cell, either from a stack or by allocating a new cell
getNewIx :: (Default a, GStorable a) => SLList a -> IO CellIndex
getNewIx list = do
  let array = getArray list
      stack = getStack list
  isStackEmpty <- FS.isEmpty stack
  if isStackEmpty then do
      allocatedIx <- OV.size array
      -- once set to the default, but will soon be overwritten by another value, which is a waste
      OV.pushBack array def
      return allocatedIx
    else
      FS.pop stack

-- |insert an element to just before where pointed by the iterator
insert :: (Default a, GStorable a) => Iterator a -> a -> IO ()
insert itr e = do
  let list = getList itr
      leftIx = getPrevIx itr
      array = getArray list
  leftCell <- OV.unsafeRead array leftIx
  let rightIx = getNextIx leftCell
  newIx <- getNewIx list
  -- for the left cell, updating by the same value is a waste
  OV.unsafeWrite array leftIx $ leftCell { getNextIx = newIx }
  OV.unsafeWrite array newIx Cell { getNextIx = rightIx, getValue = e }

-- |delete a cell from the list and push the cell index to the stack
delete :: (Default a, GStorable a) => Iterator a -> IO ()
delete itr = do
  let list = getList itr
      leftIx = getPrevIx itr
      array = getArray list
  leftCell <- OV.unsafeRead array leftIx
  let thisIx = getNextIx leftCell
  thisCell <- OV.unsafeRead array thisIx
  let rightIx = getNextIx thisCell
  -- for the left cell, updating by the same value is a waste
  OV.unsafeWrite array leftIx $ leftCell { getNextIx = rightIx }
  let stack = getStack list
  FS.push stack thisIx

forM_ :: (Default a, GStorable a) => (a -> IO ()) -> SLList a -> IO ()
forM_ f list = do
  let f' itr = do
        value <- (***) itr
        f value
  forItrM_ f' $ getBeginItr list

forItrM_ :: (Default a, GStorable a) => (Iterator a -> IO ()) -> Iterator a -> IO ()
forItrM_ f itr = do
  isItrEnd <- isEnd itr
  if isItrEnd
    then return ()
    else do
      f itr
      nextItr <- getNextItr itr
      forItrM_ f nextItr

