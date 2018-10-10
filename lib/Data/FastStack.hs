{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-
Module : Data.FastStack
Description : A very fast stack using foreign memory.

This is an implemention of stack data structure using
foreign memory. The features are

- not garbage collected, so could be very large
- very fast (but can hold only unboxed types)

-}
module Data.FastStack (
  FastStack,
  new,
  length,
  null,
  push,
  pop
)
where

import Prelude hiding (length, null)
import Control.DeepSeq
import Foreign.Storable
import qualified Data.OffHeapVector as OV

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

newtype FastStack a = FastStack (OV.OffHeapVector a) deriving (Eq, NFData)

new :: Storable a => Int -> IO (FastStack a)
new = fmap FastStack . OV.new

length :: Storable a => FastStack a -> IO Int
length (FastStack ov) = OV.length ov

null :: Storable a => FastStack a -> IO Bool
null (FastStack ov) = OV.null ov

push :: Storable a => FastStack a -> a -> IO ()
push (FastStack ov) = OV.pushBack ov

pop :: Storable a => FastStack a -> IO a
pop (FastStack ov) = OV.popBack ov
