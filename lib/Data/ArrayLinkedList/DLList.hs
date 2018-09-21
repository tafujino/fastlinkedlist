{-
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
-}

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
    ImmutableIterator(),
    Iterator,
    RIterator,
    unsafeFreeze,
    unsafeThaw,
    element,
    unsafeElement,
    beginItr,
    rBeginItr,
    endItr,
    rEndItr

{-
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
--    sentinelIx,
--    CellIndex,
--    CellSize
  )
where

{-
Module      : Data.ArrayLinkedList.DLList
Description : A fast doubly linked list implemented with an unboxed array

A doubly linked list implemented using an unboxed array.
An array is expanded to the double size when it overflows.
A dummy node is introduced for speed.
-}

import Prelude hiding (foldl, foldr, mapM_)
--import qualified Data.OffHeapVector as OV
--import qualified Data.FastStack as FS
import Control.Monad hiding (mapM_)
import qualified Data.ArrayLinkedList.DLList.Mutable as MDL
import Data.ArrayLinkedList.DLList.IteratorDirection
import Data.ArrayLinkedList.DLList.Ix
import Data.Default
--import Data.IORef
--import Data.Kind
--import Control.Monad.Trans.Maybe
--import Control.Monad.Trans.Class
import Foreign.CStorable
--import Foreign.Storable
import GHC.Generics
import System.IO.Unsafe

newtype DLList a = DLList (MDL.MDLList a)

toMutableList :: (Default a, CStorable a) => DLList a -> MDL.MDLList a
toMutableList (DLList mdl) = mdl

toImmutableList :: (Default a, CStorable a) => MDL.MDLList a -> DLList a
toImmutableList = DLList

unsafeFreeze :: (Default a, CStorable a) => MDL.MDLList a -> IO (DLList a)
unsafeFreeze = return . toImmutableList

unsafeThaw :: (Default a, CStorable a) => DLList a -> IO (MDL.MDLList a)
unsafeThaw = return . toMutableList

--------------------------------------------------------------------------------

newtype ImmutableIterator j (d :: Direction) a = ImmutableIterator (j d a) deriving Eq

type Iterator  a = ImmutableIterator MDL.MutableIterator Forward a
type RIterator a = ImmutableIterator MDL.MutableIterator Reverse a

class (Default a, CStorable a, MDL.MDLListIterator j (d :: Direction) a) => DLListIterator i j d a where
  toMutableIterator   :: i j d a -> j d a
  toImmutableIterator :: j d a -> i j d a

  thisIx :: i j d a -> CellIndex
  thisIx = MDL.thisIx . toMutableIterator

  thisList :: i j d a -> DLList a
  thisList = DLList . MDL.thisList . toMutableIterator

  unsafeElement :: i j d a -> a
  unsafeElement = unsafeDupablePerformIO . MDL.unsafeRead . toMutableIterator

  -- | Obtain the value of the cell pointed by the iterator
  element :: i j d a -> a
  element = unsafeDupablePerformIO . MDL.read . toMutableIterator

  unsafePrevItr :: i j d a -> i j d a
  unsafePrevItr = toImmutableIterator . unsafeDupablePerformIO . MDL.unsafePrevItr . toMutableIterator

  prevItr :: i j d a -> Maybe (i j d a)
  prevItr = fmap toImmutableIterator . unsafeDupablePerformIO . MDL.prevItr . toMutableIterator

  unsafeNextItr :: i j d a -> i j d a
  unsafeNextItr = toImmutableIterator . unsafeDupablePerformIO . MDL.unsafeNextItr . toMutableIterator

  nextItr :: i j d a -> Maybe (i j d a)
  nextItr = fmap toImmutableIterator . unsafeDupablePerformIO . MDL.nextItr . toMutableIterator

instance (Default a, CStorable a) => DLListIterator ImmutableIterator MDL.MutableIterator Forward a  where
  toMutableIterator :: Iterator a -> MDL.MIterator a
  toMutableIterator (ImmutableIterator mitr) = mitr

  toImmutableIterator :: MDL.MIterator a -> Iterator a
  toImmutableIterator = ImmutableIterator

instance (Default a, CStorable a) => DLListIterator ImmutableIterator MDL.MutableIterator Reverse a  where
  toMutableIterator :: RIterator a -> MDL.MRIterator a
  toMutableIterator (ImmutableIterator mitr) = mitr

  toImmutableIterator :: MDL.MRIterator a -> RIterator a
  toImmutableIterator = ImmutableIterator


beginItr :: (Default a, CStorable a) => DLList a -> Iterator a
beginItr = ImmutableIterator . unsafeDupablePerformIO . MDL.beginItr . toMutableList

rBeginItr :: (Default a, CStorable a) => DLList a -> RIterator a
rBeginItr = ImmutableIterator . unsafeDupablePerformIO . MDL.rBeginItr . toMutableList

endItr :: (Default a, CStorable a) => DLList a -> Iterator a
endItr = ImmutableIterator . MDL.endItr . toMutableList

rEndItr :: (Default a, CStorable a) => DLList a -> RIterator a
rEndItr = ImmutableIterator . MDL.rEndItr . toMutableList


-- DList cannot be Foldable, due to the constrant (Default a, CStorable a)

--foldrItr :: (Default a, CStorable a) => (a -> b -> b) -> b -> RIterator a -> b
--foldrItr f z itr = maybe id (flip $ foldrItr f) (nextItr itr) $ f (unsafeElement itr) z

--foldr :: (Default a, CStorable a) => (a -> b -> b) -> b -> DLList a -> b
--foldr f z l = foldrItr f z $ rBeginItr l

--foldl :: (Default a, CStorable a) => (b -> a -> b) -> b -> DLList a -> b
--foldl f z l = foldlItr f z $ beginItr l

foldlMItr :: (Default a, CStorable a, Monad m) => (b -> a -> m b) -> b -> Iterator a -> m b
foldlMItr f z itr = maybe return (flip $ foldlMItr f) (nextItr itr) =<< f z (unsafeElement itr)
{-
  z' <- f z $ unsafeElement itr
  maybe (return z') (foldlMItr f z') $ nextItr itr
-}
--  case nextItr itr of
--    Just itr' -> foldlMItr f z' itr'
--    Nothing   -> return z'

foldlM :: (Default a, CStorable a, Monad m) => (b -> a -> m b) -> b -> DLList a -> m b
foldlM f z list = foldlMItr f z $ beginItr list

foldrMItr :: (Default a, CStorable a, Monad m) => (a -> b -> m b) -> b -> RIterator a -> m b
foldrMItr f z itr = maybe return (flip $ foldrMItr f) (nextItr itr) =<< f (unsafeElement itr) z

foldrM :: (Default a, CStorable a, Monad m) => (a -> b -> m b) -> b -> DLList a -> m b
foldrM f z list = foldrMItr f z $ rBeginItr list

-- to do: foldMap to foldr and foldl

mapM_ :: (Default a, CStorable a, Monad m) => (a -> m b) -> DLList a -> m ()
mapM_ f l = void $ foldlM (const $ void . f) () l
-- another impl. mapM_ f l = void $ foldlM (((void . f) .) . flip const) () l

forM_ :: (Default a, CStorable a, Monad m) => DLList a -> (a -> m b) -> m ()
forM_ = flip mapM_





-- implement safe freeze and thaw (copy all the elements of list)


{-

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
