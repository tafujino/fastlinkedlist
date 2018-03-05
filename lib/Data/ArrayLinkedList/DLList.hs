{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# OPTIONS_GHC -fplugin=Foreign.Storable.Generic.Plugin #-}
{-# OPTIONS_GHC -fplugin-opt=Foreign.Storable.Generic.Plugin:-v1 #-}

module Data.ArrayLinkedList.DLList
  (
    DLList(),
    new,
    Iterator(),
    RIterator(),
    deref,
    rDeref,
    unsafeDeref,
    unsafeRDeref,
    getBeginItr,
    getRBeginItr,
    getNextItr,
    rGetNextItr,
    getPrevItr,
    rGetPrevItr,
    unsafeGetNextItr,
    unsafeRGetNextItr,
    unsafeGetPrevItr,
    unsafeRGetPrevItr,
    insert,
    rInsert,
    delete,
    rDelete,
    unsafeDelete,
    unsafeRDelete,
    pushFront,
    pushBack,
    popFront,
    popBack,
    unsafePopFront,
    unsafePopBack,
    foldlM,
    foldlM_,
    foldrM,
    foldrM_,
    forM_,
    mapM_,
    toListM,
  )
where

{-
Module      : Data.ArrayLinkedList.DLList
Description : A fast doubly linked list implemented with an unboxed array

A doubly linked list implemented using an unboxed array.
An array is expanded to the double size when it overflows.
A dummy node is introduced for speed.
-}

import Prelude hiding(mapM_)
import qualified Data.OffHeapVector as OV
import qualified Data.FastStack as FS
import Foreign.Storable
import Foreign.Storable.Generic
import GHC.Generics
import Data.Default
import Data.IORef
import Control.Monad hiding(forM_, mapM_)

type CellIndex = Int -- index 0 refer to the sentinel
type CellSize  = Int

sentinelIx :: CellIndex
sentinelIx = 0

-- a cell in an array
data Cell a = Cell {
  getPrevIx :: !CellIndex,
  getNextIx :: !CellIndex,
  getValue  :: !a
  } deriving (Show, Generic, GStorable, Default)

data DLList a = DLList {
  getArray :: !(OV.OffHeapVector (Cell a)),
  getStack :: !(FS.FastStack CellIndex)
  }

data Iterator a = Iterator {
  getList   :: !(DLList a),
  getThisIx :: !CellIndex
  }

data RIterator a = RIterator {
  rGetList   :: !(DLList a),
  rGetThisIx :: !CellIndex
  }

-- |
-- >>> list <- Data.ArrayLinkedList.DLList.new 10 :: IO (Data.ArrayLinkedList.DLList.DLList Int)
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
-- >>> pushBack list 0
-- >>> toListM list
-- [3,2,1,0]
-- >>> popFront list
-- Just 3
-- >>> toListM list
-- [2,1,0]
-- >>> i0 <- getBeginItr list
-- >>> Just i1 <- getNextItr i0
-- >>> insert i1 100
-- >>> toListM list
-- [2,100,1,0]
-- >>> ri0 <- getRBeginItr list
-- >>> Just ri1 <- rGetNextItr ri0
-- >>> Just ri2 <- rDelete ri1
-- >>> toListM list
-- [2,100,0]
-- >>> rInsert ri2 (-1)
-- >>> toListM list
-- [2,100,-1,0]

-- |
-- >>> list <- Data.ArrayLinkedList.DLList.new 10 :: IO (Data.ArrayLinkedList.DLList.DLList Int)
-- >>> toListM list
-- []
-- >>> forM_ list print
-- >>> foldlM (\x y -> return $ x + y) 0 list
-- 0
-- >>> i0 <- getBeginItr list
-- >>> deref i0
-- Nothing
-- >>> mi1 <- getNextItr i0
-- >>> :{
-- >>> case mi1 of
-- >>>   Nothing -> "Nothing"
-- >>>   Just _  -> "Just an iterator"
-- >>> :}
-- "Nothing"
--
-- >>> mi2 <- getPrevItr i0
-- >>> :{
-- >>> case mi2 of
-- >>>   Nothing -> "Nothing"
-- >>>   Just _  -> "Just an iterator"
-- >>> :}
-- "Nothing"
--
-- >>> popFront list
-- Nothing
-- >>> popBack list
-- Nothing

-- |
-- >>> list <- Data.ArrayLinkedList.DLList.new 10 :: IO (Data.ArrayLinkedList.DLList.DLList Int)
-- >>> pushFront list 1
-- >>> pushFront list 2
-- >>> pushFront list 3
-- >>> toListM list
-- [3,2,1]
-- >>> pushBack list 0
-- >>> toListM list
-- [3,2,1,0]
-- >>> popFront list
-- Just 3
-- >>> toListM list
-- [2,1,0]
-- >>> i0 <- getBeginItr list
-- >>> i1 <- unsafeGetNextItr i0
-- >>> insert i1 100
-- >>> toListM list
-- [2,100,1,0]
-- >>> ri0 <- getRBeginItr list
-- >>> ri1 <- unsafeRGetNextItr ri0
-- >>> ri2 <- unsafeRDelete ri1
-- >>> toListM list
-- [2,100,0]
-- >>> rInsert ri2 (-1)
-- >>> toListM list
-- [2,100,-1,0]


new :: (Default a, GStorable a) => CellSize -> IO (DLList a)
new initialCapacity = do
  array <- OV.new (initialCapacity + 1) -- one cell is additionaly reserved for the sentinel
  stack <- FS.new initialCapacity
  OV.pushBack array Cell { getPrevIx = sentinelIx,
                           getNextIx = sentinelIx,
                           getValue = def }
  return $ DLList array stack

-- |obtain an index of a cell, either from a stack or by allocating a new cell
getNewIx :: (Default a, GStorable a) => DLList a -> IO CellIndex
getNewIx list = do
  let array = getArray list
      stack = getStack list
  isStackEmpty <- FS.isEmpty stack
  if isStackEmpty
    then do
      allocatedIx <- OV.size array
      -- once set to the default, but will soon be overwritten by another value, which is a waste
      OV.pushBack array def
      return allocatedIx
    else
      FS.pop stack

-- |(private) obtain the cell the iterator points
itrToCell :: (Default a, GStorable a) => Iterator a -> IO (Cell a)
itrToCell itr = OV.unsafeRead (getArray (getList itr)) (getThisIx itr)

-- |(private) obtain the cell the reverse iterator points
rItrToCell :: (Default a, GStorable a) => RIterator a -> IO (Cell a)
rItrToCell itr = OV.unsafeRead (getArray (rGetList itr)) (rGetThisIx itr)

-- |obtain the value of the cell the iterator points
deref :: (Default a, GStorable a) => Iterator a -> IO (Maybe a)
deref itr = do
  let ix = getThisIx itr
  if ix == sentinelIx
    then return Nothing
    else do
      cell <- itrToCell itr
      return $ Just $ getValue cell

-- |obtain the value of the cell the reverse iterator points
rDeref :: (Default a, GStorable a) => RIterator a -> IO (Maybe a)
rDeref itr = do
  let ix = rGetThisIx itr
  if ix == sentinelIx
    then return Nothing
    else do
      cell <- rItrToCell itr
      return $ Just $ getValue cell

unsafeDeref :: (Default a, GStorable a) => Iterator a -> IO a
unsafeDeref itr = itrToCell itr >>= return . getValue

unsafeRDeref :: (Default a, GStorable a) => RIterator a -> IO a
unsafeRDeref itr = rItrToCell itr >>= return . getValue

getBeginItr :: (Default a, GStorable a) => DLList a -> IO (Iterator a)
getBeginItr list = do
  sentinelCell <- OV.unsafeRead (getArray list) sentinelIx
  return $ Iterator { getList = list, getThisIx = getNextIx sentinelCell }

getRBeginItr :: (Default a, GStorable a) => DLList a -> IO (RIterator a)
getRBeginItr list = do
  sentinelCell <- OV.unsafeRead (getArray list) sentinelIx
  return $ RIterator { rGetList = list, rGetThisIx = getPrevIx sentinelCell }

getPrevItr :: (Default a, GStorable a) => Iterator a -> IO (Maybe (Iterator a))
getPrevItr itr = do
  cell <- itrToCell itr
  let prevIx = getPrevIx cell
  return $ if prevIx == sentinelIx
    then Nothing
    else Just $ itr { getThisIx = prevIx }

getNextItr :: (Default a, GStorable a) => Iterator a -> IO (Maybe (Iterator a))
getNextItr itr = do
  if getThisIx itr == sentinelIx
    then return Nothing
    else do
      cell <- itrToCell itr
      return $ Just $ itr { getThisIx = getNextIx cell }

unsafeGetPrevItr :: (Default a, GStorable a) => Iterator a -> IO (Iterator a)
unsafeGetPrevItr itr = do
  cell <- itrToCell itr
  return $ itr { getThisIx = getPrevIx cell }

unsafeGetNextItr :: (Default a, GStorable a) => Iterator a -> IO (Iterator a)
unsafeGetNextItr itr = do
  cell <- itrToCell itr
  return $ itr { getThisIx = getNextIx cell }

rGetPrevItr :: (Default a, GStorable a) => RIterator a -> IO (Maybe (RIterator a))
rGetPrevItr itr = do
  cell <- rItrToCell itr
  let nextIx = getNextIx cell
  return $ if nextIx == sentinelIx
    then Nothing
    else Just $ itr { rGetThisIx = nextIx }

rGetNextItr :: (Default a, GStorable a) => RIterator a -> IO (Maybe (RIterator a))
rGetNextItr itr = do
  if rGetThisIx itr == sentinelIx
    then return Nothing
    else do
      cell <- rItrToCell itr
      return $ Just $ itr { rGetThisIx = getPrevIx cell }

unsafeRGetPrevItr :: (Default a, GStorable a) => RIterator a -> IO (RIterator a)
unsafeRGetPrevItr itr = do
  cell <- rItrToCell itr
  return $ itr { rGetThisIx = getNextIx cell }

unsafeRGetNextItr :: (Default a, GStorable a) => RIterator a -> IO (RIterator a)
unsafeRGetNextItr itr = do
  cell <- rItrToCell itr
  return $ itr { rGetThisIx = getPrevIx cell }

-- |(private) insert an element between two indices, which should be adjacent
insertByIx :: (Default a, GStorable a) => DLList a -> CellIndex -> CellIndex -> a -> IO ()
insertByIx list leftIx rightIx e = do
  let array = getArray list
  newIx <- getNewIx list
  leftCell  <- OV.unsafeRead array leftIx
  OV.unsafeWrite array leftIx  $ leftCell  { getNextIx = newIx }
  rightCell <- OV.unsafeRead array rightIx
  OV.unsafeWrite array rightIx $ rightCell { getPrevIx = newIx }
  OV.unsafeWrite array newIx Cell { getPrevIx = leftIx, getNextIx = rightIx, getValue = e }

-- |insert an element to just before where the iterator points (the left cell of the pointed cell)
insert :: (Default a, GStorable a) => Iterator a -> a -> IO ()
insert itr e = do
  cell <- itrToCell itr
  insertByIx (getList itr) (getPrevIx cell) (getThisIx itr) e

-- |insert an element to just before where the reverse iterator points (the right cell of the pointed cell)
rInsert :: (Default a, GStorable a) => RIterator a -> a -> IO ()
rInsert itr e = do
  cell <- rItrToCell itr
  insertByIx (rGetList itr) (rGetThisIx itr) (getNextIx cell) e

-- |(private) delete a cell of the given index from the list and push the cell index to the stack
deleteByIx :: (Default a, GStorable a) => DLList a -> CellIndex -> IO ()
deleteByIx list ix = do
  let array = getArray list
  thisCell <- OV.unsafeRead array ix
  let leftIx  = getPrevIx thisCell
      rightIx = getNextIx thisCell
  leftCell <- OV.unsafeRead array leftIx
  rightCell <- OV.unsafeRead array rightIx
  OV.unsafeWrite array leftIx  $ leftCell  { getNextIx = rightIx }
  OV.unsafeWrite array rightIx $ rightCell { getPrevIx = leftIx }
  let stack = getStack list
  FS.push stack ix

-- |delete a cell pointed by the iterator from the list and push the cell index to the stack
delete :: (Default a, GStorable a) => Iterator a -> IO (Maybe (Iterator a))
delete itr = do
  let thisIx = getThisIx itr
  mitr <- getNextItr itr
  when (thisIx /= sentinelIx) $ deleteByIx (getList itr) thisIx
  return mitr

unsafeDelete :: (Default a, GStorable a) => Iterator a -> IO (Iterator a)
unsafeDelete itr = do
  nextItr <- unsafeGetNextItr itr
  deleteByIx (getList itr) (getThisIx itr)
  return nextItr

-- |delete a cell pointed by the reverse iterator from the list and push the cell index to the stack
rDelete :: (Default a, GStorable a) => RIterator a -> IO (Maybe (RIterator a))
rDelete itr = do
  let thisIx = rGetThisIx itr
  mitr <- rGetNextItr itr
  when (thisIx /= sentinelIx) $ deleteByIx (rGetList itr) thisIx
  return mitr

unsafeRDelete :: (Default a, GStorable a) => RIterator a -> IO (RIterator a)
unsafeRDelete itr = do
  nextItr <- unsafeRGetNextItr itr
  deleteByIx (rGetList itr) (rGetThisIx itr)
  return nextItr

pushFront :: (Default a, GStorable a) => DLList a -> a -> IO ()
pushFront list e = getBeginItr list >>= (\itr -> insert itr e)

pushBack :: (Default a, GStorable a) => DLList a -> a -> IO ()
pushBack list e = getRBeginItr list >>= (\itr -> rInsert itr e)

popFront :: (Default a, GStorable a) => DLList a -> IO (Maybe a)
popFront list = do
  itr <- getBeginItr list
  mv <- deref itr
  case mv of
    Nothing -> return Nothing
    Just _  -> deleteByIx list (getThisIx itr) >> return mv

unsafePopFront :: (Default a, GStorable a) => DLList a -> IO a
unsafePopFront list = do
  itr <- getBeginItr list
  v <- unsafeDeref itr
  deleteByIx list (getThisIx itr)
  return v

popBack :: (Default a, GStorable a) => DLList a -> IO (Maybe a)
popBack list = do
  itr <- getRBeginItr list
  mv <- rDeref itr
  case mv of
    Nothing -> return Nothing
    Just _  -> deleteByIx list (rGetThisIx itr) >> return mv

unsafePopBack :: (Default a, GStorable a) => DLList a -> IO a
unsafePopBack list = do
  itr <- getRBeginItr list
  v <- unsafeRDeref itr
  deleteByIx list (rGetThisIx itr)
  return v

foldlItrM :: (Default a, GStorable a) => (b -> Iterator a -> IO b) -> b -> Iterator a -> IO b
foldlItrM f z itr = do
  if getThisIx itr == sentinelIx
    then return z
    else do
      z' <- f z itr
      nextItr <- unsafeGetNextItr itr
      foldlItrM f z' nextItr

foldlM :: (Default a, GStorable a) => (b -> a -> IO b) -> b -> DLList a -> IO b
foldlM f z list = getBeginItr list >>= foldlItrM f' z
  where f' w itr = unsafeDeref itr >>= f w

foldrItrM :: (Default a, GStorable a) => (RIterator a -> b -> IO b) -> b -> RIterator a -> IO b
foldrItrM f z itr = do
  if rGetThisIx itr == sentinelIx
    then return z
    else do
      z' <- f itr z
      nextItr <- unsafeRGetNextItr itr
      foldrItrM f z' nextItr

foldrM :: (Default a, GStorable a) => (a -> b -> IO b) -> b -> DLList a -> IO b
foldrM f z list = getRBeginItr list >>= foldrItrM f' z
  where f' itr w = unsafeRDeref itr >>= \v -> f v w

foldlM_ :: (Default a, GStorable a) => (b -> a -> IO b) -> b -> DLList a -> IO ()
foldlM_ f z list = foldlM f z list >> return ()

foldrM_ :: (Default a, GStorable a) => (a -> b -> IO b) -> b -> DLList a -> IO ()
foldrM_ f z list = foldrM f z list >> return ()

forItrM_ :: (Default a, GStorable a) => Iterator a -> (Iterator a -> IO ()) -> IO ()
forItrM_ itr f = foldlItrM f' () itr
  where f' w itr' = f itr' >> return w

forM_ :: (Default a, GStorable a) => DLList a -> (a -> IO ()) -> IO ()
forM_ list f = foldlM f' () list
  where f' w v = f v >> return w

mapM_ :: (Default a, GStorable a) => (a -> IO ()) -> DLList a -> IO ()
mapM_ f list = forM_ list f

toListM :: (Default a, GStorable a) => DLList a -> IO [a]
toListM list = foldrM (\x xs -> return $ x:xs) [] list
