{-# LANGUAGE BangPatterns #-}

{-
Module : Data.FastStack
Description : A very fast stack using foreign memory.

This is an implemention of stack data structure using
foreign memory. The features are

- not garbage collected, so could be very large
- very fast (but can hold only unboxed types)

-}
module Data.FastStack (
  FastStack(),
  new,
  size,
  isEmpty,
  push,
  pop
)
where

import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable
import qualified Data.OffHeapVector as OV
import Control.Monad
import Data.IORef

{-
 Cells:
      # = cellCapacity
             |
  /~~~~~~~~~~~~~~~~~~~\
  +---+---+---+---+---+
  |   |   |   |   |   |
  +---+---+---+---+---+
                ^
                |
            newCellIndex : this cell is returned when a new cell is allocated (when the recycle bin is empty).

 Recycle bin (stack):
      # = stackCapacity ( == cellCapacity)
             |
  /~~~~~~~~~~~~~~~~~~~\
  +---+---+---+---+---+
  |   |   |   |   |   |
  +---+---+---+---+---+
            ^
            |
          topIndex : an index integer is stored if a new cell is deallocated.
-}

data FastStack a = FastStack (OV.OffHeapVector a)

new :: Storable a => Int -> IO (FastStack a)
new initialCapacity = do
  ov <- OV.new initialCapacity
  return $ FastStack ov

size :: Storable a => (FastStack a) -> IO Int
size (FastStack ov) = OV.size ov

isEmpty :: Storable a => (FastStack a) -> IO Bool
isEmpty (FastStack ov) = OV.isEmpty ov

push :: Storable a => FastStack a -> a -> IO ()
push (FastStack ov) e = OV.pushBack ov e

pop :: Storable a => FastStack a -> IO a
pop (FastStack ov) = OV.popBack ov

  


  
  
