{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-
Module : Data.OffHeapVector
Description : A very fast off-heap vector using foreign memory.

This is an implemention of stack data structure using
foreign memory. The features are

- not garbage collected, so could be very large
- very fast (but can hold only unboxed types)

-}

module Data.OffHeapVector (
  OffHeapVector,
  new,
  null,
  length,
  unsafeRead,
  read,
  unsafeWrite,
  write,
  unsafeModify,
  modify,
  pushBack,
  popBack,
  unsafePopBack,
  )
where

import Prelude hiding (length, null, read)
import Control.DeepSeq
import Control.Monad.Extra
import Data.IORef
import qualified Data.Vector.Storable.Mutable as SMV
import GHC.Generics
import Foreign.ForeignPtr
import Foreign.Storable
import Text.Printf (printf)

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

type VecIx   = Int
type VecSize = Int

-- |
data OffHeapVector a = OffHeapVector {
  vRef    :: !(IORef (ForeignPtr a)),
  capRef  :: !(IORef VecSize),
  sizeRef :: !(IORef VecSize)
  } deriving (Eq, Generic, NFData)

-- |
-- >>> v <- new 10 :: IO (OffHeapVector Char)
-- >>> null v
-- True
-- >>> pushBack v 'a'
-- >>> null v
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

new :: Storable a => VecSize -> IO (OffHeapVector a)
new cap = do
  v       <- mallocForeignPtrArray cap
  vRef    <- newIORef v
  capRef  <- newIORef cap
  sizeRef <- newIORef 0
  return $ OffHeapVector vRef capRef sizeRef

length :: Storable a => OffHeapVector a -> IO VecSize
length = readIORef . sizeRef

null :: Storable a => OffHeapVector a -> IO Bool
null = fmap (== 0) . length

expand :: Storable a => OffHeapVector a -> VecSize -> IO ()
expand ov@(OffHeapVector vRef capRef sizeRef) deltaCap = do
  v    <- readIORef vRef
  cap  <- readIORef capRef
  size <- readIORef sizeRef
  let cap' = cap + deltaCap
  v' <- mallocForeignPtrArray cap'
  let srcVec = SMV.unsafeFromForeignPtr0 v  size
      dstVec = SMV.unsafeFromForeignPtr0 v' size
  SMV.copy dstVec srcVec
  writeIORef vRef v'
  writeIORef capRef cap'

checkBoundary :: Storable a => OffHeapVector a -> VecIx -> IO ()
checkBoundary ov@(OffHeapVector vRef _ sizeRef) ix = do
  v    <- readIORef vRef
  size <- readIORef sizeRef
  when (ix < 0 || size <= ix) $ error $ printf "Out of range (%d is out of [0, %d) )" ix size

{-# INLINE unsafeRead #-}
unsafeRead :: Storable a => OffHeapVector a -> VecIx -> IO a
unsafeRead (OffHeapVector vRef _ _) ix = do
  v <- readIORef vRef
  withForeignPtr v $ \p -> peekElemOff p ix

read :: Storable a => OffHeapVector a -> VecIx -> IO a
read ov ix = checkBoundary ov ix >> unsafeRead ov ix

unsafeWrite :: Storable a => OffHeapVector a -> VecIx -> a -> IO ()
unsafeWrite (OffHeapVector vRef _ _) ix e = do
  v <- readIORef vRef
  withForeignPtr v $ \p -> pokeElemOff p ix e

write :: Storable a => OffHeapVector a -> VecIx -> a -> IO ()
write ov ix e = checkBoundary ov ix >> unsafeWrite ov ix e

unsafeModify :: Storable a => OffHeapVector a -> (a -> a) -> VecIx -> IO ()
unsafeModify ov f ix = unsafeWrite ov ix . f =<< unsafeRead ov ix

modify :: Storable a => OffHeapVector a -> (a -> a) -> VecIx -> IO ()
modify ov f ix = checkBoundary ov ix >> unsafeModify ov f ix

pushBack :: Storable a => OffHeapVector a -> a -> IO ()
pushBack ov@(OffHeapVector vRef capRef sizeRef) e = do
  v    <- readIORef vRef
  size <- readIORef sizeRef
  cap  <- readIORef capRef
  when (size == cap) $ expand ov cap -- expand the vector to the double size
  unsafeWrite ov size e
  modifyIORef' sizeRef (+ 1)

unsafePopBack :: Storable a => OffHeapVector a -> IO a
unsafePopBack ov@(OffHeapVector _ _ sizeRef) = do
  size <- readIORef sizeRef
  e <- unsafeRead ov $ size - 1
  modifyIORef' sizeRef (subtract 1)
  return e

popBack :: Storable a => OffHeapVector a -> IO a
popBack ov = ifM (null ov) (error "OffHeapVector is Empty") $ unsafePopBack ov
