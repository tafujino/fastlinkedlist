--{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
--{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Data.ArrayLinkedList.DLList
  (
    DLList(),
    Iterator(..),
--    deref,
--    rDeref,
--    unsafeDeref,
--    unsafeRDeref,
{-    
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
-}
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

import Prelude hiding (foldl, foldr)
--import qualified Data.OffHeapVector as OV
--import qualified Data.FastStack as FS
import qualified Data.ArrayLinkedList.DLList.Mutable as MDL
import Foreign.CStorable
import Foreign.Storable
import GHC.Generics
import Data.ArrayLinkedList.DLList.IteratorDirection
import Data.Default
import Data.IORef
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import System.IO.Unsafe

type CellIndex = Int
type CellSize  = Int

sentinelIx :: CellIndex
sentinelIx = MDL.sentinelIx

newtype DLList a = DLList (MDL.MDLList a) deriving Eq

-- | (internal)
toMutableList :: (Default a, CStorable a) => DLList a -> MDL.MDLList a
toMutableList (DLList mdl) = mdl

toImmutableList :: (Default a, CStorable a) => MDL.MDLList a -> DLList a
toImmutableList = DLList

unsafeFreeze :: (Default a, CStorable a) => MDL.MDLList a -> IO (DLList a)
unsafeFreeze = return . DLList

unsafeThaw :: (Default a, CStorable a) => DLList a -> IO (MDL.MDLList a)
unsafeThaw = return . toMutableList

--------------------------------------------------------------------------------

--newtype Iterator (d :: Direction) a = Iterator (MDL.MIterator (d :: Direction) a) deriving Eq
newtype Iterator j (d :: Direction) a = Iterator (j d a) deriving Eq

--type FIterator a = Iterator (MDL.MFIterator a) Forward a
--type RIterator a = Iterator (MDL.MRIterator a) Reverse a

type FIterator a = Iterator MDL.MIterator Forward a
type RIterator a = Iterator MDL.MIterator Reverse a

class (Default a, CStorable a, MDL.MDLListIterator j d a) => DLListIterator i j d a where
  toMutableIterator   :: i j d a -> j d a
  toImmutableIterator :: j d a -> i j d a
  
  unsafeDeref :: i j d a -> a
  unsafeDeref itr = unsafeDupablePerformIO $ MDL.unsafeRead $ toMutableIterator itr

  

instance (Default a, CStorable a) => DLListIterator Iterator MDL.MIterator Forward a  where
  toMutableIterator :: FIterator a -> MDL.MFIterator a
  toMutableIterator (Iterator mitr) = mitr

  toImmutableIterator :: MDL.MFIterator a -> FIterator a
  toImmutableIterator = Iterator

instance (Default a, CStorable a) => DLListIterator Iterator MDL.MIterator Reverse a  where
  toMutableIterator :: RIterator a -> MDL.MRIterator a
  toMutableIterator (Iterator mitr) = mitr

  toImmutableIterator :: MDL.MRIterator a -> RIterator a
  toImmutableIterator = Iterator

  
  
  

{-
class (Default a, CStorable a) => DLListIterator i (d :: Direction) a where
  toMutableIterator :: (MDL.MDLListIterator j d a) => i d a -> j d a
  unsafeDeref :: (Default a, CStorable a) => i d a -> a
  unsafeDeref itr = unsafeDupablePerformIO $ MDL.unsafeRead mitr

    where
      mitr = toMutableIterator itr

instance (Default a, CStorable a) => DLListIterator Iterator Forward a where
  toMutableIterator :: Iterator Forward a -> MDL.MIterator Forward a
  toMutableIterator (Iterator mitr) = mitr

instance (Default a, CStorable a) => DLListIterator Iterator Reverse a where
  toMutableIterator :: Iterator Reverse a -> MDL.MIterator Reverse a
  toMutableIterator (Iterator mitr) = mitr
-}  

--instance (Default a, CStorable a, MDL.MDLListIterator j) => DLListIterator Iterator MDL.MIterator a where
--  toMutableIterator :: Iterator a -> MDL.MIterator a
--  toMutableIterator (Iterator mitr) = mitr
  
  

{-
class (Default a, CStorable a) => DLListIterator i (d :: Direction) a where
  toMutableIterator :: i d a -> MDL.MIterator d a
--  toImmutableIterator :: (Default a, CStorable a) => MDL.MIterator d a -> Iterator d a
  unsafeDeref :: (Default a, CStorable a) => i d a -> a  

instance (Default a, CStorable a) => DLListIterator Iterator (d :: Direction) a where
  toMutableIterator :: (Default a, CStorable a) => Iterator d a -> MDL.MIterator d a
  toMutableIterator (Iterator mitr) = mitr

--  toImmutableIterator :: (Default a, CStorable a) => MDL.MIterator d a -> Iterator d a
--  toImmutableIterator mitr = Iterator mitr :: Iterator d a

  unsafeDeref :: (Default a, CStorable a) => Iterator d a -> a
  unsafeDeref = unsafeDupablePerformIO . MDL.unsafeRead . toMutableIterator
-}

--toMutableIterator :: (Default a, CStorable a) => Iterator (d :: Direction) a -> MDL.MIterator (d :: Direction) a
--toMutableIterator (Iterator mitr) = mitr

--toImmutableIterator :: (Default a, CStorable a) => MDL.MIterator d a -> Iterator d a
--toImmutableIterator = Iterator





{-
getBeginItr :: (Default a, CStorable a) => DLList a -> Iterator a
getBeginItr dl =
  Iterator { getList   = dl,
             getThisIx = MDL.getThisIx $ unsafeDupablePerformIO $ MDL.getBeginItr $ toMutableList dl
           }

getEndItr :: (Default a, CStorable a) => DLList a -> Iterator a
getEndItr dl =
  Iterator { getList   = dl,
             getThisIx = sentinelIx
           }
-}

--unsafeDeref :: (Default a, CStorable a) => Iterator a -> a
--unsafeDeref = unsafeDupablePerformIO . MDL.unsafeRead . toMutableIterator

{-
-- |obtain the value of the cell the iterator points
deref :: (Default a, CStorable a) => Iterator a -> Maybe a
deref = unsafeDupablePerformIO . MDL.read . toMutableIterator

unsafePrevItr :: (Default a, CStorable a) => Iterator a -> Iterator a
unsafePrevItr = toImmutableIterator . unsafeDupablePerformIO . MDL.unsafePrevItr . toMutableIterator

prevItr :: (Default a, CStorable a) => Iterator a -> Maybe (Iterator a)
prevItr = fmap toImmutableIterator . unsafeDupablePerformIO . MDL.prevItr . toMutableIterator

unsafeNextItr :: (Default a, CStorable a) => Iterator a -> Iterator a
unsafeNextItr = toImmutableIterator . unsafeDupablePerformIO . MDL.unsafeNextItr . toMutableIterator

nextItr :: (Default a, CStorable a) => Iterator a -> Maybe (Iterator a)
nextItr = fmap toImmutableIterator . unsafeDupablePerformIO . MDL.nextItr . toMutableIterator



-}




-- implement safe freeze and thaw (copy all the elements of list)


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

{-

-- |(private) obtain the cell the iterator points
itrToCell :: (Default a, CStorable a) => Iterator a -> IO (Cell a)
itrToCell itr = OV.unsafeRead (getArray $ getList itr) (getThisIx itr)

-- |(private) obtain the cell the reverse iterator points
rItrToCell :: (Default a, CStorable a) => RIterator a -> IO (Cell a)
rItrToCell itr = OV.unsafeRead (getArray $ rGetList itr) (rGetThisIx itr)

-- |obtain the value of the cell the iterator points
deref :: (Default a, CStorable a) => Iterator a -> IO (Maybe a)
deref itr = runMaybeT $ do
  guard $ getThisIx itr /= sentinelIx
  getValue <$> lift (itrToCell itr)

-- |obtain the value of the cell the reverse iterator points
rDeref :: (Default a, CStorable a) => RIterator a -> IO (Maybe a)
rDeref itr = runMaybeT $ do
  guard $ rGetThisIx itr /= sentinelIx
  getValue <$> lift (rItrToCell itr)

unsafeDeref :: (Default a, CStorable a) => Iterator a -> IO a
unsafeDeref itr = getValue <$> itrToCell itr

unsafeRDeref :: (Default a, CStorable a) => RIterator a -> IO a
unsafeRDeref itr = getValue <$> rItrToCell itr

getBeginItr :: (Default a, CStorable a) => DLList a -> IO (Iterator a)
getBeginItr list = do
  sentinelCell <- OV.unsafeRead (getArray list) sentinelIx
  return Iterator { getList = list, getThisIx = getNextIx sentinelCell }

getRBeginItr :: (Default a, CStorable a) => DLList a -> IO (RIterator a)
getRBeginItr list = do
  sentinelCell <- OV.unsafeRead (getArray list) sentinelIx
  return RIterator { rGetList = list, rGetThisIx = getPrevIx sentinelCell }

getEndItr :: (Default a, CStorable a) => DLList a -> Iterator a
getEndItr list = Iterator { getList = list, getThisIx = sentinelIx }

getREndItr :: (Default a, CStorable a) => DLList a -> RIterator a
getREndItr list = RIterator { rGetList = list, rGetThisIx = sentinelIx }

getPrevItr :: (Default a, CStorable a) => Iterator a -> IO (Maybe (Iterator a))
getPrevItr itr = runMaybeT $ do
  prevIx <- getPrevIx <$> lift (itrToCell itr)
  guard $ prevIx /= sentinelIx
  return itr { getThisIx = prevIx }

getNextItr :: (Default a, CStorable a) => Iterator a -> IO (Maybe (Iterator a))
getNextItr itr = runMaybeT $ do
  guard $ getThisIx itr /= sentinelIx
  nextIx <- getNextIx <$> lift (itrToCell itr)
  return itr { getThisIx = nextIx }

unsafeGetPrevItr :: (Default a, CStorable a) => Iterator a -> IO (Iterator a)
unsafeGetPrevItr itr = do
  cell <- itrToCell itr
  return itr { getThisIx = getPrevIx cell }

unsafeGetNextItr :: (Default a, CStorable a) => Iterator a -> IO (Iterator a)
unsafeGetNextItr itr = do
  cell <- itrToCell itr
  return itr { getThisIx = getNextIx cell }

rGetPrevItr :: (Default a, CStorable a) => RIterator a -> IO (Maybe (RIterator a))
rGetPrevItr itr = runMaybeT $ do
  nextIx <- getNextIx <$> lift (rItrToCell itr)
  guard $ nextIx /= sentinelIx
  return itr { rGetThisIx = nextIx }

rGetNextItr :: (Default a, CStorable a) => RIterator a -> IO (Maybe (RIterator a))
rGetNextItr itr = runMaybeT $ do
  guard $ rGetThisIx itr /= sentinelIx
  prevIx <- getPrevIx <$> lift (rItrToCell itr)
  return itr { rGetThisIx = prevIx }

unsafeRGetPrevItr :: (Default a, CStorable a) => RIterator a -> IO (RIterator a)
unsafeRGetPrevItr itr = do
  cell <- rItrToCell itr
  return itr { rGetThisIx = getNextIx cell }

unsafeRGetNextItr :: (Default a, CStorable a) => RIterator a -> IO (RIterator a)
unsafeRGetNextItr itr = do
  cell <- rItrToCell itr
  return itr { rGetThisIx = getPrevIx cell }


foldlItr :: (Default a, CStorable a) => (b -> Iterator a -> IO b) -> b -> Iterator a -> IO b
foldlItr f z itr
  | getThisIx itr == sentinelIx = return z
  | otherwise = do
      z' <- f z itr  -- (1)
      nextItr <- unsafeGetNextItr itr  -- (2)
      foldlItr f z' nextItr
-- if you want to delete the cell in f, (1) and (2) should be exchanged

foldlIO :: (Default a, CStorable a) => (b -> a -> IO b) -> b -> DLList a -> IO b
foldlIO f z = foldlItr f' z <=< getBeginItr
  where f' w = f w <=< unsafeDeref

foldlIO_ :: (Default a, CStorable a) => (b -> a -> IO b) -> b -> DLList a -> IO ()
foldlIO_ f z list = void $ foldlIO f z list

foldl :: (Default a, CStorable a) => (b -> a -> b) -> b -> DLList a -> IO b
foldl f = foldlIO $ (return .) . f

foldl1 :: (Default a, CStorable a) => (a -> a -> a) -> DLList a -> IO (Maybe a)
foldl1 f list = runMaybeT $ do
  itr0 <- lift $ getBeginItr list
  v <- MaybeT $ deref itr0
  itr1 <- lift $ unsafeGetNextItr itr0
  lift $ foldlItr f' v itr1
    where f' w itr = f w <$> unsafeDeref itr

foldrItr :: (Default a, CStorable a) => (RIterator a -> b -> IO b) -> b -> RIterator a -> IO b
foldrItr f z itr
  | rGetThisIx itr == sentinelIx = return z
  | otherwise = do
      z' <- f itr z
      nextItr <- unsafeRGetNextItr itr
      foldrItr f z' nextItr

foldrIO :: (Default a, CStorable a) => (a -> b -> IO b) -> b -> DLList a -> IO b
foldrIO f z = getRBeginItr >=> foldrItr f' z
  where f' itr w = unsafeRDeref itr >>= flip f w

foldrIO_ :: (Default a, CStorable a) => (a -> b -> IO b) -> b -> DLList a -> IO ()
foldrIO_ f z list = void $ foldrIO f z list

foldr :: (Default a, CStorable a) => (a -> b -> b) -> b -> DLList a -> IO b
foldr f = foldrIO $ (return .) . f

foldr1 :: (Default a, CStorable a) => (a -> a -> a) -> DLList a -> IO (Maybe a)
foldr1 f list = runMaybeT $ do
  itr0 <- lift $ getRBeginItr list
  v <- MaybeT $ rDeref itr0
  itr1 <- lift $ unsafeRGetNextItr itr0
  lift $ foldrItr f' v itr1 
    where f' itr w = flip f w <$> unsafeRDeref itr 

forItr_ :: (Default a, CStorable a) => Iterator a -> (Iterator a -> IO ()) -> IO ()
forItr_ itr f = foldlItr (const $ void . f) () itr
-- (another impl.) forItr_ itr f = foldlItr (\x y -> f y >> return x) () itr

forIO_ :: (Default a, CStorable a) => DLList a -> (a -> IO ()) -> IO ()
forIO_ list f = foldlIO_ (const $ void . f) () list
-- (another impl.) forIO_ list f = foldlIO_ (\x y -> f y >> return x) () list

mapIO_ :: (Default a, CStorable a) => (a -> IO ()) -> DLList a -> IO ()
mapIO_ = flip forIO_

toList :: (Default a, CStorable a) => DLList a -> IO [a]
toList = foldrIO ((return .) . (:)) []
-}
