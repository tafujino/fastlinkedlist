{-
Module : Data.OffHeapVector.Mutable
Description : A very fast off-heap vector using foreign memory.

This is an implemention of stack data structure using
foreign memory. The features are

- not garbage collected, so could be very large
- very fast (but can hold only unboxed types)

-}

module Data.OffHeapVector.Mutable (
  MVector,
  new,
  null,
  length,
  read,
  write,
  unsafeRead,
  unsafeWrite,
  pushBack,
  popBack,
  unsafePopBack,
  clone
  )
where

import Prelude hiding (length, null, read)
import Control.Monad.Extra
import Data.IORef
import qualified Data.Vector.Storable.Mutable as SMV
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
data MVector a = MVector {
  vRef    :: !(IORef (ForeignPtr a)),
  capRef  :: !(IORef VecSize),
  sizeRef :: !(IORef VecSize)
  } deriving Eq

length :: Storable a => MVector a -> IO VecSize
length = readIORef . sizeRef

-- |
-- >>> v <- new 10 :: IO (MVector Char)
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


null :: Storable a => MVector a -> IO Bool
null = fmap (== 0) . length

new :: Storable a => VecSize -> IO (MVector a)
new cap = do
  v       <- mallocForeignPtrArray cap
  vRef    <- newIORef v
  capRef  <- newIORef cap
  sizeRef <- newIORef 0
  return $ MVector vRef capRef sizeRef


expand :: Storable a => MVector a -> VecSize -> IO ()
expand ov@(MVector vRef capRef sizeRef) deltaCap = do
  v    <- readIORef vRef
  cap  <- readIORef capRef
  size <- readIORef sizeRef
  let cap' = cap + deltaCap
  v' <- copyContent v cap' size
  writeIORef vRef v'
  writeIORef capRef cap'

{-
expand :: Storable a => MVector a -> VecSize -> IO ()
expand (MVector vRef capRef sizeRef) deltaCap = do
  cap  <- readIORef capRef
  let cap' = cap + deltaCap
  v    <- readIORef vRef
  v'   <- mallocForeignPtrArray cap'
  size <- readIORef sizeRef
  let srcVec = SMV.unsafeFromForeignPtr0 v  size
      dstVec = SMV.unsafeFromForeignPtr0 v' size
  SMV.copy dstVec srcVec
  writeIORef vRef v'
  writeIORef capRef cap'
-}  

checkBoundary :: Storable a => MVector a -> VecIx -> IO ()
checkBoundary ov@(MVector vRef _ sizeRef) ix = do
  v    <- readIORef vRef
  size <- readIORef sizeRef
  when (ix < 0 || size <= ix) $ error $ printf "Out of range (%d is out of [0, %d) )" ix size

unsafeRead :: Storable a => MVector a -> VecIx -> IO a
unsafeRead (MVector vRef _ _) ix = do
  v <- readIORef vRef
  withForeignPtr v $ \p -> peekElemOff p ix

read :: Storable a => MVector a -> VecIx -> IO a
read ov@(MVector vRef _ sizeRef) ix = do
  checkBoundary ov ix
  unsafeRead ov ix

unsafeWrite :: Storable a => MVector a -> VecIx -> a -> IO ()
unsafeWrite (MVector vRef _ _) ix e = do
  v <- readIORef vRef
  withForeignPtr v $ \p -> pokeElemOff p ix e

write :: Storable a => MVector a -> VecIx -> a -> IO ()
write ov@(MVector vRef _ sizeRef) ix e = do
  checkBoundary ov ix
  unsafeWrite ov ix e

pushBack :: Storable a => MVector a -> a -> IO ()
pushBack ov@(MVector vRef capRef sizeRef) e = do
  v    <- readIORef vRef
  size <- readIORef sizeRef
  cap  <- readIORef capRef
  when (size == cap) $ expand ov cap -- expand the vector to the double size
  unsafeWrite ov size e
  modifyIORef' sizeRef (+ 1)

unsafePopBack :: Storable a => MVector a -> IO a
unsafePopBack ov@(MVector _ _ sizeRef) = do
  size <- readIORef sizeRef
  e <- unsafeRead ov $ size - 1
  modifyIORef' sizeRef (subtract 1)
  return e

popBack :: Storable a => MVector a -> IO a
popBack ov = ifM (null ov) (error "OffHeapVector is Empty") $ unsafePopBack ov

copyContent :: Storable a => ForeignPtr a -> VecSize -> VecSize -> IO (ForeignPtr a)
copyContent v cap size = do
  v' <- mallocForeignPtrArray cap
  let srcVec = SMV.unsafeFromForeignPtr0 v  size
      dstVec = SMV.unsafeFromForeignPtr0 v' size
  SMV.copy dstVec srcVec
  return v'

clone :: Storable a => MVector a -> IO (MVector a)
clone ov@(MVector vRef capRef sizeRef) = do
  v        <- readIORef vRef
  size     <- readIORef sizeRef
  v'       <- copyContent v size size
  vRef'    <- newIORef v'
  capRef'  <- newIORef size
  sizeRef' <- newIORef size
  return $ MVector vRef' capRef' sizeRef'
