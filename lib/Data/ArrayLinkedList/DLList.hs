{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# OPTIONS_GHC -fplugin=Foreign.Storable.Generic.Plugin #-}
{-# OPTIONS_GHC -fplugin-opt=Foreign.Storable.Generic.Plugin:-v1 #-}

module Data.ArrayLinkedList.DLList
  (
    DLList(),
    new,
    Iterator(..),
    RIterator(..),
    deref,
    rDeref,
    unsafeDeref,
    unsafeRDeref,
    getBeginItr,
    getRBeginItr,
    getEndItr,
    getREndItr,
    getNextItr,
    rGetNextItr,
    getPrevItr,
    rGetPrevItr,
    unsafeGetNextItr,
    unsafeRGetNextItr,
    unsafeGetPrevItr,
    unsafeRGetPrevItr,
    unsafeWrite,
    write,
    unsafeModify,
    modify,
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
    foldlItr,
    foldl,
    foldlIO,
    foldlIO_,
    foldrItr,
    foldr,
    foldrIO,
    foldrIO_,
    forIO_,
    mapIO_,
    toList,
    sentinelIx,
    CellIndex,
    CellSize
  )
where

{-
Module      : Data.ArrayLinkedList.DLList
Description : A fast doubly linked list implemented with an unboxed array

A doubly linked list implemented using an unboxed array.
An array is expanded to the double size when it overflows.
A dummy node is introduced for speed.
-}

import Prelude hiding(foldl, foldr)
import qualified Data.OffHeapVector as OV
import qualified Data.FastStack as FS
import Foreign.Storable
import Foreign.Storable.Generic
import GHC.Generics
import Data.Default
import Data.IORef
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class

type CellIndex = Int
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
  } deriving Eq

data Iterator a = Iterator {
  getList   :: !(DLList a),
  getThisIx :: !CellIndex
  } deriving Eq

data RIterator a = RIterator {
  rGetList   :: !(DLList a),
  rGetThisIx :: !CellIndex
  } deriving Eq

-- |
-- >>> list <- Data.ArrayLinkedList.DLList.new 10 :: IO (Data.ArrayLinkedList.DLList.DLList Int)
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
-- >>> list <- Data.ArrayLinkedList.DLList.new 10 :: IO (Data.ArrayLinkedList.DLList.DLList Int)
-- >>> toList list
-- []
-- >>> forIO_ list print
-- >>> foldl (+) 0 list
-- 0
-- >>> i0 <- getBeginItr list
-- >>> deref i0
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
-- >>> list <- Data.ArrayLinkedList.DLList.new 10 :: IO (Data.ArrayLinkedList.DLList.DLList Int)
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


new :: (Default a, GStorable a) => CellSize -> IO (DLList a)
new initialCapacity = do
  array <- OV.new $ initialCapacity + 1 -- one cell is additionaly reserved for the sentinel
  stack <- FS.new initialCapacity
  OV.pushBack array Cell { getPrevIx = sentinelIx,
                           getNextIx = sentinelIx,
                           getValue = def }
  return $ DLList array stack

-- | obtain an index of a cell, either from a stack or by allocating a new cell
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
    else FS.pop stack

-- |(private) obtain the cell the iterator points
itrToCell :: (Default a, GStorable a) => Iterator a -> IO (Cell a)
itrToCell itr = OV.unsafeRead (getArray $ getList itr) (getThisIx itr)

-- |(private) obtain the cell the reverse iterator points
rItrToCell :: (Default a, GStorable a) => RIterator a -> IO (Cell a)
rItrToCell itr = OV.unsafeRead (getArray $ rGetList itr) (rGetThisIx itr)

-- |obtain the value of the cell the iterator points
deref :: (Default a, GStorable a) => Iterator a -> IO (Maybe a)
deref itr = runMaybeT $ do
  guard $ getThisIx itr /= sentinelIx
  getValue <$> lift (itrToCell itr)

-- |obtain the value of the cell the reverse iterator points
rDeref :: (Default a, GStorable a) => RIterator a -> IO (Maybe a)
rDeref itr = runMaybeT $ do
  guard $ rGetThisIx itr /= sentinelIx
  getValue <$> lift (rItrToCell itr)

unsafeDeref :: (Default a, GStorable a) => Iterator a -> IO a
unsafeDeref itr = getValue <$> itrToCell itr

unsafeRDeref :: (Default a, GStorable a) => RIterator a -> IO a
unsafeRDeref itr = getValue <$> rItrToCell itr

getBeginItr :: (Default a, GStorable a) => DLList a -> IO (Iterator a)
getBeginItr list = do
  sentinelCell <- OV.unsafeRead (getArray list) sentinelIx
  return Iterator { getList = list, getThisIx = getNextIx sentinelCell }

getRBeginItr :: (Default a, GStorable a) => DLList a -> IO (RIterator a)
getRBeginItr list = do
  sentinelCell <- OV.unsafeRead (getArray list) sentinelIx
  return RIterator { rGetList = list, rGetThisIx = getPrevIx sentinelCell }

getEndItr :: (Default a, GStorable a) => DLList a -> Iterator a
getEndItr list = Iterator { getList = list, getThisIx = sentinelIx }

getREndItr :: (Default a, GStorable a) => DLList a -> RIterator a
getREndItr list = RIterator { rGetList = list, rGetThisIx = sentinelIx }

getPrevItr :: (Default a, GStorable a) => Iterator a -> IO (Maybe (Iterator a))
getPrevItr itr = runMaybeT $ do
  prevIx <- getPrevIx <$> lift (itrToCell itr)
  guard $ prevIx /= sentinelIx
  return itr { getThisIx = prevIx }

getNextItr :: (Default a, GStorable a) => Iterator a -> IO (Maybe (Iterator a))
getNextItr itr = runMaybeT $ do
  guard $ getThisIx itr /= sentinelIx
  nextIx <- getNextIx <$> lift (itrToCell itr)
  return itr { getThisIx = nextIx }

unsafeGetPrevItr :: (Default a, GStorable a) => Iterator a -> IO (Iterator a)
unsafeGetPrevItr itr = do
  cell <- itrToCell itr
  return itr { getThisIx = getPrevIx cell }

unsafeGetNextItr :: (Default a, GStorable a) => Iterator a -> IO (Iterator a)
unsafeGetNextItr itr = do
  cell <- itrToCell itr
  return itr { getThisIx = getNextIx cell }

rGetPrevItr :: (Default a, GStorable a) => RIterator a -> IO (Maybe (RIterator a))
rGetPrevItr itr = runMaybeT $ do
  nextIx <- getNextIx <$> lift (rItrToCell itr)
  guard $ nextIx /= sentinelIx
  return itr { rGetThisIx = nextIx }

rGetNextItr :: (Default a, GStorable a) => RIterator a -> IO (Maybe (RIterator a))
rGetNextItr itr = runMaybeT $ do
  guard $ rGetThisIx itr /= sentinelIx
  prevIx <- getPrevIx <$> lift (rItrToCell itr)
  return itr { rGetThisIx = prevIx }

unsafeRGetPrevItr :: (Default a, GStorable a) => RIterator a -> IO (RIterator a)
unsafeRGetPrevItr itr = do
  cell <- rItrToCell itr
  return itr { rGetThisIx = getNextIx cell }

unsafeRGetNextItr :: (Default a, GStorable a) => RIterator a -> IO (RIterator a)
unsafeRGetNextItr itr = do
  cell <- rItrToCell itr
  return itr { rGetThisIx = getPrevIx cell }

unsafeWrite :: (Default a, GStorable a) => Iterator a -> a -> IO ()
unsafeWrite itr e = do
  let array = getArray $ getList itr
      ix = getThisIx itr
  cell <- OV.unsafeRead array ix
  OV.unsafeWrite array ix cell { getValue = e }

write :: (Default a, GStorable a) => Iterator a -> a -> IO ()
write itr e = do
  let array = getArray $ getList itr
      ix = getThisIx itr
  when (ix /= sentinelIx) $ error "cannot update the value of sentinel cell"
  cell <- OV.unsafeRead array ix
  OV.unsafeWrite array ix cell { getValue = e }

unsafeModify :: (Default a, GStorable a) => Iterator a -> (a -> a) -> IO ()
unsafeModify itr f = unsafeWrite itr =<< f <$> unsafeDeref itr

modify :: (Default a, GStorable a) => Iterator a -> (a -> a) -> IO ()
modify itr f = do
  let array = getArray $ getList itr
      ix = getThisIx itr
  when (ix /= sentinelIx) $ error "cannot update the value of sentinel cell"
  unsafeModify itr f

-- |(private) insert an element between two indices, which should be adjacent
insertByIx :: (Default a, GStorable a) => DLList a -> CellIndex -> CellIndex -> a -> IO CellIndex
insertByIx list leftIx rightIx e = do
  let array = getArray list
  newIx <- getNewIx list
  leftCell  <- OV.unsafeRead array leftIx
  OV.unsafeWrite array leftIx  $ leftCell  { getNextIx = newIx }
  rightCell <- OV.unsafeRead array rightIx
  OV.unsafeWrite array rightIx $ rightCell { getPrevIx = newIx }
  OV.unsafeWrite array newIx Cell { getPrevIx = leftIx, getNextIx = rightIx, getValue = e }
  return newIx

-- |insert an element to just before where the iterator points (the left cell of the pointed cell)
insert :: (Default a, GStorable a) => Iterator a -> a -> IO (Iterator a)
insert itr e = do
  cell <- itrToCell itr
  ix <- insertByIx (getList itr) (getPrevIx cell) (getThisIx itr) e
  return itr { getThisIx = ix }

-- |insert an element to just before where the reverse iterator points (the right cell of the pointed cell)
rInsert :: (Default a, GStorable a) => RIterator a -> a -> IO (RIterator a)
rInsert itr e = do
  cell <- rItrToCell itr
  ix <- insertByIx (rGetList itr) (rGetThisIx itr) (getNextIx cell) e
  return itr { rGetThisIx = ix }

-- |(private) delete a cell of the given index from the list and push the cell index to the stack
deleteByIx :: (Default a, GStorable a) => DLList a -> CellIndex -> IO ()
deleteByIx list ix = do
  let array = getArray list
  thisCell <- OV.unsafeRead array ix
  let leftIx  = getPrevIx thisCell
      rightIx = getNextIx thisCell
  leftCell <- OV.unsafeRead array leftIx
  rightCell <- OV.unsafeRead array rightIx
  OV.unsafeWrite array leftIx  leftCell  { getNextIx = rightIx }
  OV.unsafeWrite array rightIx rightCell { getPrevIx = leftIx }
  FS.push (getStack list) ix

-- |delete a cell pointed by the iterator from the list and push the cell index to the stack
delete :: (Default a, GStorable a) => Iterator a -> IO (Maybe (Iterator a))
delete itr = runMaybeT $ do
  let thisIx = getThisIx itr
  guard $ thisIx /= sentinelIx
  nextItr <- lift $ unsafeGetNextItr itr
  lift $ deleteByIx (getList itr) thisIx
  return nextItr

unsafeDelete :: (Default a, GStorable a) => Iterator a -> IO (Iterator a)
unsafeDelete itr = do
  nextItr <- unsafeGetNextItr itr
  deleteByIx (getList itr) (getThisIx itr)
  return nextItr

-- |delete a cell pointed by the reverse iterator from the list and push the cell index to the stack
rDelete :: (Default a, GStorable a) => RIterator a -> IO (Maybe (RIterator a))
rDelete itr = runMaybeT $ do
  let thisIx = rGetThisIx itr
  guard $ thisIx /= sentinelIx
  nextItr <- lift $ unsafeRGetNextItr itr
  lift $ deleteByIx (rGetList itr) thisIx
  return nextItr

unsafeRDelete :: (Default a, GStorable a) => RIterator a -> IO (RIterator a)
unsafeRDelete itr = do
  nextItr <- unsafeRGetNextItr itr
  deleteByIx (rGetList itr) (rGetThisIx itr)
  return nextItr

pushFront :: (Default a, GStorable a) => DLList a -> a -> IO ()
pushFront list e = void $ (`insert` e) =<< getBeginItr list

pushBack :: (Default a, GStorable a) => DLList a -> a -> IO ()
pushBack list e = void $ (`rInsert` e) =<< getRBeginItr list

popFront :: (Default a, GStorable a) => DLList a -> IO (Maybe a)
popFront list = runMaybeT $ do
  itr <- lift $ getBeginItr list
  v <- MaybeT $ deref itr
  lift $ deleteByIx list $ getThisIx itr
  return v

unsafePopFront :: (Default a, GStorable a) => DLList a -> IO a
unsafePopFront list = do
  itr <- getBeginItr list
  v <- unsafeDeref itr
  deleteByIx list $ getThisIx itr
  return v

popBack :: (Default a, GStorable a) => DLList a -> IO (Maybe a)
popBack list = runMaybeT $ do
  itr <- lift $ getRBeginItr list
  v <- MaybeT $ rDeref itr
  lift $ deleteByIx list $ rGetThisIx itr
  return v

unsafePopBack :: (Default a, GStorable a) => DLList a -> IO a
unsafePopBack list = do
  itr <- getRBeginItr list
  v <- unsafeRDeref itr
  deleteByIx list $ rGetThisIx itr
  return v

foldlItr :: (Default a, GStorable a) => (b -> Iterator a -> IO b) -> b -> Iterator a -> IO b
foldlItr f z itr
  | getThisIx itr == sentinelIx = return z
  | otherwise = do
      z' <- f z itr  -- (1)
      nextItr <- unsafeGetNextItr itr  -- (2)
      foldlItr f z' nextItr
-- if you want to delete the cell in f, (1) and (2) should be exchanged

foldlIO :: (Default a, GStorable a) => (b -> a -> IO b) -> b -> DLList a -> IO b
foldlIO f z = foldlItr f' z <=< getBeginItr
  where f' w = f w <=< unsafeDeref

foldlIO_ :: (Default a, GStorable a) => (b -> a -> IO b) -> b -> DLList a -> IO ()
foldlIO_ f z list = void $ foldlIO f z list

foldl :: (Default a, GStorable a) => (b -> a -> b) -> b -> DLList a -> IO b
foldl f = foldlIO $ (return .) . f

foldl1 :: (Default a, GStorable a) => (a -> a -> a) -> DLList a -> IO (Maybe a)
foldl1 f list = runMaybeT $ do
  itr0 <- lift $ getBeginItr list
  v <- MaybeT $ deref itr0
  itr1 <- lift $ unsafeGetNextItr itr0
  lift $ foldlItr f' v itr1
    where f' w itr = f w <$> unsafeDeref itr

foldrItr :: (Default a, GStorable a) => (RIterator a -> b -> IO b) -> b -> RIterator a -> IO b
foldrItr f z itr
  | rGetThisIx itr == sentinelIx = return z
  | otherwise = do
      z' <- f itr z
      nextItr <- unsafeRGetNextItr itr
      foldrItr f z' nextItr

foldrIO :: (Default a, GStorable a) => (a -> b -> IO b) -> b -> DLList a -> IO b
foldrIO f z = getRBeginItr >=> foldrItr f' z
  where f' itr w = unsafeRDeref itr >>= flip f w

foldrIO_ :: (Default a, GStorable a) => (a -> b -> IO b) -> b -> DLList a -> IO ()
foldrIO_ f z list = void $ foldrIO f z list

foldr :: (Default a, GStorable a) => (a -> b -> b) -> b -> DLList a -> IO b
foldr f = foldrIO $ (return .) . f

foldr1 :: (Default a, GStorable a) => (a -> a -> a) -> DLList a -> IO (Maybe a)
foldr1 f list = runMaybeT $ do
  itr0 <- lift $ getRBeginItr list
  v <- MaybeT $ rDeref itr0
  itr1 <- lift $ unsafeRGetNextItr itr0
  lift $ foldrItr f' v itr1 
    where f' itr w = flip f w <$> unsafeRDeref itr 

forItr_ :: (Default a, GStorable a) => Iterator a -> (Iterator a -> IO ()) -> IO ()
forItr_ itr f = foldlItr (const $ void . f) () itr
-- (another impl.) forItr_ itr f = foldlItr (\x y -> f y >> return x) () itr

forIO_ :: (Default a, GStorable a) => DLList a -> (a -> IO ()) -> IO ()
forIO_ list f = foldlIO_ (const $ void . f) () list
-- (another impl.) forIO_ list f = foldlIO_ (\x y -> f y >> return x) () list

mapIO_ :: (Default a, GStorable a) => (a -> IO ()) -> DLList a -> IO ()
mapIO_ = flip forIO_

toList :: (Default a, GStorable a) => DLList a -> IO [a]
toList = foldrIO ((return .) . (:)) []
