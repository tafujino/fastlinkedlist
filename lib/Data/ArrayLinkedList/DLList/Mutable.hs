{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

--{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
--{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
--{-# LANGUAGE ScopedTypeVariables #-}
--{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Data.ArrayLinkedList.DLList.Mutable
  (
    MDLList(),
    MDLListIterator,
    MIterator(),
    MFIterator,
    MRIterator,
    new,
    beginItr,
    rBeginItr,
    endItr,
    rEndItr,
    thisIx,
    thisList,
--    prevIx,
--    nextIx,
--    setIx,
    unsafePrevItr,
    prevItr,
    unsafeNextItr,
    nextItr,
    unsafeRead,
    read,
    unsafeWrite,
    modify,
    unsafeModify,
    insert,
    unsafeDelete,
    delete,
    pushFront,
    pushBack,
    unsafePopFront,
    popFront,
    unsafePopBack,
    popBack
  )
where

{-
Module      : Data.ArrayLinkedList.DLList.Mutable
Description : A fast doubly linked list implemented with an unboxed array

A doubly linked list implemented using an unboxed array.
An array is expanded to the double size when it overflows.
A dummy node is introduced for speed.
-}

import Prelude hiding (read)
import Control.Monad
import Control.Monad.Extra
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Data.ArrayLinkedList.DLList.IteratorDirection
import Data.ArrayLinkedList.DLList.Ix
import Data.Default
import qualified Data.FastStack as FS
import Data.IORef
import qualified Data.OffHeapVector as OV
import Foreign.CStorable
import Foreign.Storable
import GHC.Generics

-- |
-- >>> list <- Data.ArrayLinkedList.DLList.Mutable.new 10 :: IO (Data.ArrayLinkedList.DLList.Mutable.MDLList Int)
-- >>> pushFront list 1
-- >>> pushFront list 2
-- >>> pushFront list 3
-- >>> toList list
-- [3,2,1]
-- >>> forIO_ list print
-- 3
-- 2
-- 1
-- >>> foldl (+) 0 list
-- 6
-- >>> pushBack list 0
-- >>> toList list
-- [3,2,1,0]
-- >>> popFront list
-- Just 3
-- >>> toList list
-- [2,1,0]
-- >>> i0 <- getBeginItr list
-- >>> Just i1 <- getNextItr i0
-- >>> insert i1 100
-- >>> toList list
-- [2,100,1,0]
-- >>> ri0 <- getRBeginItr list
-- >>> Just ri1 <- rGetNextItr ri0
-- >>> Just ri2 <- rDelete ri1
-- >>> toList list
-- [2,100,0]
-- >>> rInsert ri2 (-1)
-- >>> toList list
-- [2,100,-1,0]

-- |
-- >>> list <- Data.ArrayLinkedList.DLList.Mutable.new 10 :: IO (Data.ArrayLinkedList.DLList.Mutable.MDLList Int)
-- >>> toList list
-- []
-- >>> forIO_ list print
-- >>> foldl (+) 0 list
-- 0
-- >>> i0 <- getBeginItr list
-- >>> unsafeRead i0
-- Nothing
-- >>> mi1 <- getNextItr i0
-- >>> maybe Nothing (const $ Just "Iterator") mi1
-- Nothing
-- >>> mi2 <- getPrevItr i0
-- >>> maybe Nothing (const $ Just "Iterator") mi2
-- Nothing
-- >>> popFront list
-- Nothing
-- >>> popBack list
-- Nothing

-- |
-- >>> list <- Data.ArrayLinkedList.DLList.Mutable.new 10 :: IO (Data.ArrayLinkedList.DLList.Mutable.MDLList Int)
-- >>> pushFront list 1
-- >>> pushFront list 2
-- >>> pushFront list 3
-- >>> toList list
-- [3,2,1]
-- >>> pushBack list 0
-- >>> toList list
-- [3,2,1,0]
-- >>> unsafePopFront list
-- 3
-- >>> toList list
-- [2,1,0]
-- >>> i0 <- getBeginItr list
-- >>> i1 <- unsafeGetNextItr i0
-- >>> insert i1 100
-- >>> toList list
-- [2,100,1,0]
-- >>> ri0 <- getRBeginItr list
-- >>> ri1 <- unsafeRGetNextItr ri0
-- >>> ri2 <- unsafeRDelete ri1
-- >>> toList list
-- [2,100,0]
-- >>> rInsert ri2 (-1)
-- >>> toList list
-- [2,100,-1,0]

-- a cell in an array
data Cell a = Cell {
  leftIx    :: !CellIndex,
  rightIx   :: !CellIndex,
  cellValue :: !a
  } deriving (Show, Generic, CStorable)

instance Default a => Default (Cell a) where
  def = Cell { leftIx    = sentinelIx,
               rightIx   = sentinelIx,
               cellValue = def
             }

setCellValue :: a -> Cell a -> Cell a
setCellValue e cell = cell { cellValue = e }

instance CStorable a => Storable (Cell a) where
  peek      = cPeek
  poke      = cPoke
  alignment = cAlignment
  sizeOf    = cSizeOf

data MDLList a = MDLList {
  listVector  :: !(OV.OffHeapVector (Cell a)),
  listIxStack :: !(FS.FastStack CellIndex)
  } deriving Eq

data MIterator (d :: Direction) a = MIterator {
  itrList :: !(MDLList a),
  itrIx   :: !CellIndex
  } deriving Eq

type MFIterator a = MIterator Forward a
type MRIterator a = MIterator Reverse a

--------------------------------------------------------------------------------


-- | obtain the cell the iterator points
--itrCell :: (Default a, CStorable a) => MIterator (d :: Direction) a -> IO (Cell a)
--itrCell itr = OV.unsafeRead (listVector $ itrList itr) $ thisIx itr

class (Default a, CStorable a) => MDLListIterator i (d :: Direction) a where
  -- | primitive operations whose definition depend on the iteration direction
  direction :: i d a -> Direction
  thisList  :: i d a -> MDLList a
  prevIx    :: i d a -> IO CellIndex
  nextIx    :: i d a -> IO CellIndex
  thisIx    :: i d a -> CellIndex
  setIx     :: CellIndex -> i d a -> i d a

  ifValidItr :: (i d a -> IO b) -> i d a -> IO (Maybe b)
  ifValidItr f itr = runMaybeT $ do
    guard $ thisIx itr /= sentinelIx
    lift $ f itr

  itrCell :: i d a -> IO (Cell a)
  itrCell itr = OV.unsafeRead (listVector $ thisList itr) $ thisIx itr

  unsafePrevItr :: i d a -> IO (i d a)
  unsafePrevItr itr = flip setIx itr <$> prevIx itr

  unsafeNextItr :: i d a -> IO (i d a)
  unsafeNextItr itr = flip setIx itr <$> nextIx itr

  prevItr :: i d a -> IO (Maybe (i d a))
  prevItr = ifValidItr return <=< unsafePrevItr

  nextItr :: i d a -> IO (Maybe (i d a))
  nextItr = ifValidItr return <=< unsafeNextItr

  unsafeRead :: i d a -> IO a
  unsafeRead itr = cellValue <$> itrCell itr

  read :: i d a -> IO a
  read itr = do
    when (thisIx itr /= sentinelIx) $ error "cannot read the sentinel cell"
    unsafeRead itr

  unsafeWrite :: i d a -> a -> IO ()
  unsafeWrite itr e = do
    let vec = listVector $ thisList itr
        ix  = thisIx itr
    cell <- OV.unsafeRead vec ix
    OV.unsafeWrite vec ix cell { cellValue = e }

  write :: i d a -> a -> IO ()
  write itr e = do
    when (thisIx itr /= sentinelIx) $ error "cannot write to the sentinel cell"
    unsafeWrite itr e

  unsafeModify :: i d a -> (a -> a) -> IO ()
  unsafeModify itr f = unsafeWrite itr =<< f <$> unsafeRead itr

  modify :: i d a -> (a -> a) -> IO ()
  modify itr f = do
    when (thisIx itr /= sentinelIx) $ error "cannot modify the sentinel cell"
    unsafeModify itr f

  -- | Insert an element just before where the iterator points and retun the iterator to the inserted cell
  insert :: i d a -> a -> IO (i d a)
  insert itr e = do
    let list = thisList itr
        vec  = listVector list
        dir  = direction itr
    ix0 <- prevIx itr
    let ix1 = thisIx itr
    ix <- newIx list
    OV.unsafeModify vec (setNextCellIx dir ix) ix0
    OV.unsafeModify vec (setPrevCellIx dir ix) ix1
    OV.unsafeWrite vec ix $ setPrevCellIx dir ix0 $ setNextCellIx dir ix1 $ setCellValue e def
    return $ setIx ix itr

-- | Delete a cell pointed by the iterator, push the cell index to the stack, and
--   return the next iterator
  unsafeDelete :: i d a -> IO (i d a)
  unsafeDelete itr = do
    let list = thisList itr
        vec = listVector list
        dir = direction itr
    ix0 <- prevIx itr
    ix1 <- nextIx itr
    OV.unsafeModify vec (setNextCellIx dir ix1) ix0
    OV.unsafeModify vec (setPrevCellIx dir ix0) ix1
    FS.push (listIxStack list) $ thisIx itr
    return $ setIx ix1 itr

  delete :: i d a -> IO (Maybe (i d a))
  delete = ifValidItr unsafeDelete

--------------------------------------------------------------------------------

instance (Default a, CStorable a) => MDLListIterator MIterator Forward a where
  direction :: MFIterator a -> Direction
  direction _ = Forward

  thisList :: MFIterator a -> MDLList a
  thisList = itrList

  prevIx :: MFIterator a -> IO CellIndex
  prevIx = fmap leftIx . itrCell

  nextIx :: MFIterator a -> IO CellIndex
  nextIx = fmap rightIx . itrCell

  thisIx :: MFIterator a -> CellIndex
  thisIx = itrIx

  setIx :: CellIndex -> MFIterator a -> MFIterator a
  setIx ix itr = itr { itrIx = ix }

instance (Default a, CStorable a) => MDLListIterator MIterator Reverse a where
  direction :: MIterator Reverse a -> Direction
  direction _ = Reverse

  thisList :: MRIterator a -> MDLList a
  thisList = itrList

  prevIx :: MRIterator a -> IO CellIndex
  prevIx = fmap rightIx . itrCell

  nextIx :: MRIterator a -> IO CellIndex
  nextIx = fmap leftIx . itrCell

  thisIx :: MRIterator a -> CellIndex
  thisIx = itrIx

  setIx :: CellIndex -> MRIterator a -> MIterator d a
  setIx ix itr = itr { itrIx = ix }

--------------------------------------------------------------------------------

new :: (Default a, CStorable a) => CellSize -> IO (MDLList a)
new initialCapacity = do
  vec   <- OV.new $ initialCapacity + 1 -- one cell is additionaly reserved for the sentinel
  stack <- FS.new initialCapacity
  OV.pushBack vec Cell { leftIx    = sentinelIx,
                         rightIx   = sentinelIx,
                         cellValue = def }
  return $ MDLList vec stack

-- | obtain an index of a cell, either from a stack or by allocating a new cell
newIx :: (Default a, CStorable a) => MDLList a -> IO CellIndex
newIx list = ifM (FS.null stack)
                (do allocatedIx <- OV.length vec
                    OV.pushBack vec def
                    return allocatedIx)
                (FS.pop stack)
  where
    vec   = listVector list
    stack = listIxStack list

beginItr :: (Default a, CStorable a) => MDLList a -> IO (MIterator Forward a)
beginItr list = do
  sentinelCell <- OV.unsafeRead (listVector list) sentinelIx
  return MIterator { itrList = list, itrIx = rightIx sentinelCell }

rBeginItr :: (Default a, CStorable a) => MDLList a -> IO (MIterator Reverse a)
rBeginItr list = do
  sentinelCell <- OV.unsafeRead (listVector list) sentinelIx
  return MIterator { itrList = list, itrIx = leftIx sentinelCell }

endItr :: (Default a, CStorable a) => MDLList a -> MIterator Forward a
endItr list = MIterator { itrList = list, itrIx = sentinelIx }

rEndItr :: (Default a, CStorable a) => MDLList a -> MIterator Reverse a
rEndItr list = MIterator { itrList = list, itrIx = sentinelIx }

setPrevCellIx :: (Default a, CStorable a) => Direction -> CellIndex -> Cell a -> Cell a
setPrevCellIx Forward ix cell = cell { leftIx  = ix }
setPrevCellIx Reverse ix cell = cell { rightIx = ix }

setNextCellIx :: (Default a, CStorable a) => Direction -> CellIndex -> Cell a -> Cell a
setNextCellIx Forward ix cell = cell { rightIx = ix }
setNextCellIx Reverse ix cell = cell { leftIx  = ix }

pushFront :: (Default a, CStorable a) => MDLList a -> a -> IO ()
pushFront list e = void $ (`insert` e) =<< beginItr list

pushBack :: (Default a, CStorable a) => MDLList a -> a -> IO ()
pushBack list e = void $ (`insert` e) =<< rBeginItr list

unsafePop :: (Default a, CStorable a, MDLListIterator i d a) => i d a -> IO a
unsafePop itr = do
  e <- unsafeRead itr
  unsafeDelete itr
  return e

unsafePopFront :: (Default a, CStorable a) => MDLList a -> IO a
unsafePopFront = unsafePop <=< beginItr

popFront :: (Default a, CStorable a) => MDLList a -> IO (Maybe a)
popFront = ifValidItr unsafePop <=< beginItr

unsafePopBack :: (Default a, CStorable a) => MDLList a -> IO a
unsafePopBack = unsafePop <=< rBeginItr

popBack :: (Default a, CStorable a) => MDLList a -> IO (Maybe a)
popBack = ifValidItr unsafePop <=< rBeginItr
