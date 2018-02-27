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
    foldlM,
    foldlM_,
    mapM_,
    toListM,
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

-- implement erasure of the cells of the given range

import Prelude hiding(mapM_)
import qualified Data.OffHeapVector as OV
import qualified Data.FastStack as FS
import Foreign.Storable
import Foreign.Storable.Generic
import GHC.Generics
import Data.Default
import Data.IORef

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
  getArray :: !(OV.OffHeapVector (Cell a)),
  getStack :: !(FS.FastStack CellIndex)
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

getThisIx :: (Default a, GStorable a) => Iterator a -> IO CellIndex
getThisIx itr = do
  let list = getList itr
      array = getArray list
      prevIx = getPrevIx itr
  prevCell <- OV.unsafeRead array prevIx
  return $ getNextIx prevCell

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

(***) :: (Default a, GStorable a) => Iterator a -> IO (Maybe a)
(***) itr = do
  ix <- getThisIx itr
  if ix == sentinelIx
    then return Nothing
    else do
      let list = getList itr
          array = getArray list
      thisCell <- OV.unsafeRead array ix
      return $ Just $ getValue thisCell

getBeginItr :: (Default a, GStorable a) => SLList a -> Iterator a
getBeginItr list = Iterator { getList = list, getPrevIx = sentinelIx }

getNextItr :: (Default a, GStorable a) => Iterator a -> IO (Maybe (Iterator a))
getNextItr itr = do
  thisIx <- getThisIx itr
  return $ if thisIx == sentinelIx
    then Nothing
    else Just $ Iterator { getList = getList itr, getPrevIx = thisIx }

-- |insert an element to just before where the iterator points
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

foldlItrM :: (Default a, GStorable a) => (b -> Iterator a -> IO b) -> b -> Iterator a -> IO b
foldlItrM f z itr = do
  mNextItr <- getNextItr itr
  case mNextItr of
    Nothing      -> return z -- when this iterator points to the sentinel
    Just nextItr -> do
      z' <- f z itr
      foldlItrM f z' nextItr

foldlM :: (Default a, GStorable a) => (b -> a -> IO b) -> b -> SLList a -> IO b
foldlM f z list = foldlItrM f' z (getBeginItr list)
  where f' w itr = do
          mv <- (***) itr
          case mv of
            Nothing -> return w
            Just v  -> f w v

foldlM_ :: (Default a, GStorable a) => (b -> a -> IO b) -> b -> SLList a -> IO ()
foldlM_ f z list = foldlM f z list >> return ()

forItrM_ :: (Default a, GStorable a) => Iterator a -> (Iterator a -> IO ()) -> IO ()
forItrM_ itr f = foldlItrM f' () itr
  where f' w itr' = f itr' >> return w

forM_ :: (Default a, GStorable a) => SLList a -> (a -> IO ()) -> IO ()
forM_ list f = foldlM f' () list
  where f' w v = f v >> return w

mapM_ :: (Default a, GStorable a) => (a -> IO ()) -> SLList a -> IO ()
mapM_ f list = forM_ list f

toListM :: (Default a, GStorable a) => SLList a -> IO [a]
toListM list = do
  let f xs x = return $ x:xs
  l <- foldlM f [] list
  return $ reverse l
  

