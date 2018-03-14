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
    deref,
    unsafeDeref,
    getNextItr,
    unsafeGetNextItr,
    insert,
    delete,
    unsafeDelete,
    forM_,
    foldlM,
    foldlM_,
    mapM_,
    toListM
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
import qualified Data.DList as DL

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

-- |
-- >>> list <- Data.ArrayLinkedList.SLList.new 10 :: IO (Data.ArrayLinkedList.SLList.SLList Int)
-- >>> i0 = getBeginItr list
-- >>> deref i0
-- Nothing
-- >>> mi1 <- getNextItr i0
-- >>> maybe Nothing (\_ -> Just "Iterator") mi1
-- Nothing
-- >>> popFront list
-- Nothing
-- >>> pushFront list 1
-- >>> pushFront list 2
-- >>> pushFront list 3
-- >>> toListM list
-- [3,2,1]
-- >>> forM_ list print
-- 3
-- 2
-- 1
-- >>> foldlM (\x y -> return $ x + y) 0 list
-- 6
-- >>> i2 = getBeginItr list
-- >>> Just i3 <- getNextItr i2
-- >>> insert i3 100
-- >>> toListM list
-- [3,100,2,1]
-- >>> Just i4 <- getNextItr i3
-- >>> Just i5 <- delete i4
-- >>> toListM list
-- [3,100,1]
-- >>> pushFront list 200
-- >>> toListM list
-- [200,3,100,1]
-- >>> popFront list
-- Just 200
-- >>> toListM list
-- [3,100,1]

new :: (Default a, GStorable a) => CellSize -> IO (SLList a)
new initialCapacity = do
  array <- OV.new (initialCapacity + 1) -- one cell is additionaly reserved for a sentinel
  stack <- FS.new initialCapacity
  OV.pushBack array Cell { getNextIx = sentinelIx, getValue = def }
  return $ SLList array stack

getThisIx :: (Default a, GStorable a) => Iterator a -> IO CellIndex
getThisIx itr = OV.unsafeRead (getArray (getList itr)) (getPrevIx itr) >>= return . getNextIx

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

-- |(private) obtain the cell the iterator points
itrToCell :: (Default a, GStorable a) => Iterator a -> IO (Cell a)
itrToCell itr = do
  thisIx <- getThisIx itr
  OV.unsafeRead (getArray (getList itr)) thisIx

deref :: (Default a, GStorable a) => Iterator a -> IO (Maybe a)
deref itr = do
  ix <- getThisIx itr
  if ix == sentinelIx
    then return Nothing
    else OV.unsafeRead (getArray (getList itr)) ix >>= return . Just . getValue

unsafeDeref :: (Default a, GStorable a) => Iterator a -> IO a
unsafeDeref itr = do
  ix <- getThisIx itr
  cell <- OV.unsafeRead (getArray (getList itr)) ix
  return $ getValue cell

getBeginItr :: (Default a, GStorable a) => SLList a -> Iterator a
getBeginItr list = Iterator { getList = list, getPrevIx = sentinelIx }

getNextItr :: (Default a, GStorable a) => Iterator a -> IO (Maybe (Iterator a))
getNextItr itr = do
  thisIx <- getThisIx itr
  return $ if thisIx == sentinelIx
    then Nothing
    else Just itr { getPrevIx = thisIx }

unsafeGetNextItr :: (Default a, GStorable a) => Iterator a -> IO (Iterator a)
unsafeGetNextItr itr = do
  thisIx <- getThisIx itr
  return itr { getPrevIx = thisIx }

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
delete :: (Default a, GStorable a) => Iterator a -> IO (Maybe (Iterator a))
delete itr = do
  let list = getList itr
      leftIx = getPrevIx itr
      array = getArray list
  leftCell <- OV.unsafeRead array leftIx
  let thisIx = getNextIx leftCell
  if thisIx == sentinelIx
    then return Nothing
    else do
      thisCell <- OV.unsafeRead array thisIx
      let rightIx = getNextIx thisCell
      -- for the left cell, updating by the same value is a waste
      OV.unsafeWrite array leftIx $ leftCell { getNextIx = rightIx }
      FS.push (getStack list) thisIx
      return $ Just itr { getPrevIx = leftIx }

unsafeDelete :: (Default a, GStorable a) => Iterator a -> IO (Iterator a)
unsafeDelete itr = do
  let list = getList itr
      leftIx = getPrevIx itr
      array = getArray list
  leftCell <- OV.unsafeRead array leftIx
  let thisIx = getNextIx leftCell
  thisCell <- OV.unsafeRead array thisIx
  let rightIx = getNextIx thisCell
  -- for the left cell, updating by the same value is a waste
  OV.unsafeWrite array leftIx $ leftCell { getNextIx = rightIx }
  FS.push (getStack list) thisIx
  return itr { getPrevIx = leftIx }

pushFront :: (Default a, GStorable a) => SLList a -> a -> IO ()
pushFront list e = insert (getBeginItr list) e

unsafePopFront :: (Default a, GStorable a) => SLList a -> IO a
unsafePopFront list = do
  let itr = getBeginItr list
  e <- unsafeDeref itr
  unsafeDelete itr
  return e

popFront :: (Default a, GStorable a) => SLList a -> IO (Maybe a)
popFront list = do
  let itr = getBeginItr list
  me <- deref itr
  case me of
    Nothing -> return Nothing
    Just e  -> do
      unsafeDelete itr
      return $ Just e

foldlItrM :: (Default a, GStorable a) => (b -> Iterator a -> IO b) -> b -> Iterator a -> IO b
foldlItrM f z itr = do
  thisIx <- getThisIx itr
  if thisIx == sentinelIx
    then return z
    else f z itr >>= \z' -> foldlItrM f z' itr { getPrevIx = thisIx }

foldlM :: (Default a, GStorable a) => (b -> a -> IO b) -> b -> SLList a -> IO b
foldlM f z list = foldlItrM f' z (getBeginItr list)
  where f' w itr = unsafeDeref itr >>= f w

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
toListM list = foldlM ((return .) . DL.snoc) DL.empty list >>= return . DL.toList
