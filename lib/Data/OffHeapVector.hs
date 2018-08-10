{-
Module : Data.OffHeapVector
Description : A very fast off-heap vector using foreign memory.

This is an implemention of stack data structure using
foreign memory. The features are

- not garbage collected, so could be very large
- very fast (but can hold only unboxed types)

-}

module Data.OffHeapVector (
  OffHeapVector(),
  new,
  size,
  isEmpty,
  read,
  write,
  unsafeRead,
  unsafeWrite,
  pushBack,
  popBack
  )
where

import Prelude hiding(read)
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable
import qualified Data.Vector.Storable.Mutable as V
import Text.Printf (printf)
import Control.Monad
import Data.IORef

{-
      # = capacity
             |
  /~~~~~~~~~~~~~~~~~~~\
  +---+---+---+---+---+
  |   |   |   |   |   |
  +---+---+---+---+---+
    ^           ^
    |           |
    |        newElementIndex : this cell is used when pushBack is called.
    |
   ptr

-}

type ArrayIndex = Int

-- |
data OffHeapVector a = OffHeapVector !(IORef (ForeignPtr a)) !(IORef Int) !(IORef Int) deriving Eq
-- OffHeapvector ptr capacity size

size :: Storable a => OffHeapVector a -> IO Int
size (OffHeapVector _ _ sizeRef) = readIORef sizeRef


-- |
-- >>> v <- new 10 :: IO (OffHeapVector Char)
-- >>> isEmpty v
-- True
-- >>> pushBack v 'a'
-- >>> isEmpty v
-- False
-- >>> pushBack v 'b'
-- >>> pushBack v 'c'
-- >>> read v 0
-- 'a'
-- >>> read v 2
-- 'c'
-- >>> write v 1 'd'
-- >>> read v 1
-- 'd'



isEmpty :: Storable a => OffHeapVector a -> IO Bool
isEmpty ov = do
  sz <- size ov
  return $ if (sz == 0) then True else False

new :: Storable a => Int -> IO (OffHeapVector a)
new initialCapacity = do
  v <- mallocForeignPtrArray initialCapacity
  vRef <- newIORef v
  capRef <- newIORef initialCapacity
  sizeRef <- newIORef 0
  return $ OffHeapVector vRef capRef sizeRef

expand :: Storable a => OffHeapVector a -> Int -> IO ()
expand (OffHeapVector vRef capRef sizeRef) additionalCapacity = do
  oldCapacity <- readIORef capRef
  let newCapacity = additionalCapacity + oldCapacity
  nv <- mallocForeignPtrArray newCapacity
  oldV <- readIORef vRef
  oldSize <- readIORef sizeRef
  let srcVec = V.unsafeFromForeignPtr0 oldV oldSize
      dstVec = V.unsafeFromForeignPtr0 nv oldSize
  V.copy dstVec srcVec
  writeIORef vRef nv
  writeIORef capRef newCapacity

unsafeRead :: Storable a => OffHeapVector a -> ArrayIndex -> IO a
unsafeRead (OffHeapVector vRef _ _) ix = do
  v <- readIORef vRef
  withForeignPtr v $ \p -> peekElemOff p ix

read :: Storable a => OffHeapVector a -> ArrayIndex -> IO a
read ov@(OffHeapVector vRef _ sizeRef) ix = do
  v <- readIORef vRef
  size <- readIORef sizeRef
  when (ix < 0 || (size <= ix)) $ error $ printf "Out of range (%d is out of [0, %d) )" ix size
  unsafeRead ov ix

unsafeWrite :: Storable a => OffHeapVector a -> ArrayIndex -> a -> IO ()
unsafeWrite (OffHeapVector vRef _ _) ix e = do
  v <- readIORef vRef
  withForeignPtr v $ \p -> pokeElemOff p ix e

write :: Storable a => OffHeapVector a -> ArrayIndex -> a -> IO ()
write ov@(OffHeapVector vRef _ sizeRef) ix e = do
  v <- readIORef vRef
  size <- readIORef sizeRef
  when (ix < 0 || (size <= ix)) $ error $ printf "Out of range (%d is out of [0, %d) )" ix size
  unsafeWrite ov ix e

pushBack :: Storable a => OffHeapVector a -> a -> IO ()
pushBack ov@(OffHeapVector vRef capRef sizeRef) e = do
  v <- readIORef vRef
  size <- readIORef sizeRef
  cap <- readIORef capRef
  when (size == cap) $ expand ov cap -- expand the vector to the double size
  unsafeWrite ov size e
  modifyIORef' sizeRef (+1)

popBack :: Storable a => OffHeapVector a -> IO a
popBack ov@(OffHeapVector _ _ sizeRef) = do
  size <- readIORef sizeRef
  when (size == 0) $ error "OffHeapVector is Empty"
  e <- unsafeRead ov (size - 1)
  modifyIORef' sizeRef (subtract 1)
  return e
