{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.ArrayLinkedList.DLList
  (
    DLList,
    Iterator,
    RIterator,
    unsafeFreeze,
    unsafeThaw,
    newItr,
    newRItr,
    thisIx,
    thisList,
    element,
    unsafeElement,
    beginItr,
    rBeginItr,
    endItr,
    rEndItr,
    unsafePrevItr,
    prevItr,
    unsafeNextItr,
    nextItr,
    foldl,
    foldr,
    foldlM,
    foldM,
    foldrM,
    forM_,
    mapM_,
    toList,
    toIxList
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
import Control.Monad hiding (foldM, forM_, mapM_)
import Control.Monad.Identity hiding (foldM, forM_, mapM_)
import qualified Data.ArrayLinkedList.DLList.Mutable as MDL
import Data.ArrayLinkedList.DLList.IteratorDirection
import Data.ArrayLinkedList.DLList.Ix
import Data.Default
import Data.Proxy
import Foreign.CStorable
import GHC.Generics
import System.IO.Unsafe
--import Data.Functor.Contravariant

newtype DLList a = DLList (MDL.MDLList a)

toMutableList :: (Default a, CStorable a) => DLList a -> MDL.MDLList a
toMutableList (DLList mdl) = mdl

toImmutableList :: (Default a, CStorable a) => MDL.MDLList a -> DLList a
toImmutableList = DLList

unsafeFreeze :: (Default a, CStorable a) => MDL.MDLList a -> IO (DLList a)
unsafeFreeze = return . toImmutableList

unsafeThaw :: (Default a, CStorable a) => DLList a -> IO (MDL.MDLList a)
unsafeThaw = return . toMutableList

-- to do: implement safe freeze and thaw (copy all the elements of list)

--------------------------------------------------------------------------------

-- | Immutable iterator wraps mutable iterator (of the same direction)
newtype ImmutableIterator (j :: Direction -> * -> *) (d :: Direction) a = ImmutableIterator (j d a) deriving Eq

type Iterator  a = ImmutableIterator MDL.MutableIterator Forward a
type RIterator a = ImmutableIterator MDL.MutableIterator Reverse a

--------------------------------------------------------------------------------

class DLListMorphism t u where
  morph :: t -> u

instance DLListMorphism t t where
  morph = id

instance (DLListMorphism t u) => DLListMorphism (Maybe t) (Maybe u) where
  morph = fmap morph

instance (DLListMorphism t u) => DLListMorphism (s -> t) (s -> u) where
  morph = fmap morph

--instance (DLListMorphism a b) => DLListMorphism (a -> c) (b -> c) where
--  morph = contramap morph

instance (DLListMorphism t u) => DLListMorphism (IO t) u where
  morph = morph . unsafeDupablePerformIO

instance (Default a, CStorable a) => DLListMorphism (DLList a) (MDL.MDLList a) where
  morph = toMutableList

instance (Default a, CStorable a) => DLListMorphism (MDL.MDLList a) (DLList a) where
  morph = toImmutableList

instance (Default a, CStorable a, DLListIterator i j d a, MDL.MDLListIterator j d a) => DLListMorphism (i j d a) (j d a) where
  morph = toMutableItr

instance (Default a, CStorable a, DLListIterator i j d a, MDL.MDLListIterator j d a) => DLListMorphism (j d a) (i j d a) where
  morph = toImmutableItr

deriveMorph :: (DLListMorphism t u, DLListMorphism u' t') => (u -> u') -> (t -> t')
deriveMorph f = morph . f . morph

deriveItrMorph :: (Default a,
                   CStorable a,
                   DLListMorphism u t,
                   DLListIterator i j d a,
                   MDL.MDLListIterator j d a
                  ) => (j d a -> u) -> (i j d a -> t)
deriveItrMorph = deriveMorph

deriveListMorph :: (Default a, CStorable a, DLListMorphism u t) => (MDL.MDLList a -> u) -> (DLList a -> t)
deriveListMorph = deriveMorph

--------------------------------------------------------------------------------

class (Default a,
       CStorable a,
       MDL.MDLListIterator (j :: Direction -> * -> *)  (d :: Direction) a
      ) => DLListIterator i j d a where
  toMutableItr   :: i j d a -> j d a
  toImmutableItr :: j d a -> i j d a

  thisIx :: i j d a -> CellIndex
  thisIx = deriveItrMorph MDL.thisIx

  thisList :: i j d a -> DLList a
  thisList = deriveItrMorph MDL.thisList

  unsafeElement :: i j d a -> a
  unsafeElement = deriveItrMorph MDL.unsafeRead

  -- | Obtain the value of the cell pointed by the iterator
  element :: i j d a -> a
  element = deriveItrMorph MDL.read

  unsafePrevItr :: i j d a -> i j d a
  unsafePrevItr = deriveItrMorph MDL.unsafePrevItr

  prevItr :: i j d a -> Maybe (i j d a)
  prevItr = deriveItrMorph MDL.prevItr

  unsafeNextItr :: i j d a -> i j d a
  unsafeNextItr = deriveItrMorph MDL.unsafeNextItr

  nextItr :: i j d a -> Maybe (i j d a)
  nextItr = deriveItrMorph MDL.nextItr

--------------------------------------------------------------------------------

instance (Default a, CStorable a) => DLListIterator ImmutableIterator MDL.MutableIterator Forward a where
  toMutableItr :: Iterator a -> MDL.MIterator a
  toMutableItr (ImmutableIterator mitr) = mitr

  toImmutableItr :: MDL.MIterator a -> Iterator a
  toImmutableItr = ImmutableIterator


instance (Default a, CStorable a) => DLListIterator ImmutableIterator MDL.MutableIterator Reverse a where
  toMutableItr :: RIterator a -> MDL.MRIterator a
  toMutableItr (ImmutableIterator mitr) = mitr

  toImmutableItr :: MDL.MRIterator a -> RIterator a
  toImmutableItr = ImmutableIterator

--------------------------------------------------------------------------------

newItr :: (Default a, CStorable a) => DLList a -> CellIndex -> Iterator a
newItr = deriveListMorph MDL.newItr

newRItr :: (Default a, CStorable a) => DLList a -> CellIndex -> RIterator a
newRItr = deriveListMorph MDL.newRItr

beginItr :: (Default a, CStorable a) => DLList a -> Iterator a
beginItr = deriveListMorph MDL.beginItr

rBeginItr :: (Default a, CStorable a) => DLList a -> RIterator a
rBeginItr = deriveListMorph MDL.rBeginItr

endItr :: (Default a, CStorable a) => DLList a -> Iterator a
endItr = deriveListMorph MDL.endItr

rEndItr :: (Default a, CStorable a) => DLList a -> RIterator a
rEndItr = deriveListMorph MDL.rEndItr

-- DList cannot be Foldable, becasue the type its element is restricted to (Default a, CStorable a)

-- to do: generate foldr and foldl from foldMap

foldlMItr :: (Default a, CStorable a, Monad m) => (b -> Iterator a -> m b) -> b -> Iterator a -> m b
foldlMItr f z itr = maybe return (flip $ foldlMItr f) (nextItr itr) =<< f z itr

foldlM :: (Default a, CStorable a, Monad m) => (b -> a -> m b) -> b -> DLList a -> m b
foldlM f z l = foldlMItr (\x -> f x . unsafeElement) z $ beginItr l

foldM :: (Default a, CStorable a, Monad m) => (b -> a -> m b) -> b -> DLList a -> m b
foldM = foldlM

foldrMItr :: (Default a, CStorable a, Monad m) => (RIterator a -> b -> m b) -> b -> RIterator a -> m b
foldrMItr f z itr = maybe return (flip $ foldrMItr f) (nextItr itr) =<< f itr z

foldrM :: (Default a, CStorable a, Monad m) => (a -> b -> m b) -> b -> DLList a -> m b
foldrM f z l = foldrMItr (f . unsafeElement) z $ rBeginItr l

foldl :: (Default a, CStorable a) => (b -> a -> b) -> b -> DLList a -> b
foldl f z l = runIdentity $ foldlM ((return . ) . f) z l

foldr :: (Default a, CStorable a) => (a -> b -> b) -> b -> DLList a -> b
foldr f z l = runIdentity $ foldrM ((return . ) . f) z l

foldlItr :: (Default a, CStorable a) => (b -> Iterator a -> b) -> b -> Iterator a -> b
foldlItr f z itr = runIdentity $ foldlMItr ((return . ) . f) z itr

foldrItr :: (Default a, CStorable a) => (RIterator a -> b -> b) -> b -> RIterator a -> b
foldrItr f z itr = runIdentity $ foldrMItr ((return . ) . f) z itr

mapM_ :: (Default a, CStorable a, Monad m) => (a -> m b) -> DLList a -> m ()
mapM_ f l = foldM (const $ void . f) () l

forM_ :: (Default a, CStorable a, Monad m) => DLList a -> (a -> m b) -> m ()
forM_ = flip mapM_

toList :: (Default a, CStorable a) => DLList a -> [a]
toList l = foldr (:) [] l

-- to do: implement fromList

toIxList :: (Default a, CStorable a) => DLList a -> [CellIndex]
toIxList = foldrItr ((:) . thisIx) [] . rBeginItr
