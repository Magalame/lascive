{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Lib
    where

import Data.Primitive (Prim, ByteArray(..), MutableByteArray(..))
import qualified Data.Primitive as P

import qualified Control.Monad.Primitive as C

import qualified GHC.Prim as G
import GHC.Prim

import GHC.Base (Int(..), Double(..))
import Control.Monad.ST (ST, runST)

import Debug.Trace



data DoubleX4 = DoubleX4 DoubleX4#

instance Show DoubleX4 where
    show (DoubleX4 vec) = "(" ++ show (D# a) ++ "," ++ show (D# b) ++ "," ++ show (D# c) ++ "," ++ show (D# d) ++ ")"
        where (# a, b, c, d #) = unpackDoubleX4# vec

unDoubleX4 :: DoubleX4 -> DoubleX4#
unDoubleX4 (DoubleX4 vec) = vec



data Matrix a = Matrix {-# UNPACK #-} !Int -- number of rows
                       {-# UNPACK #-} !Int -- number of column  
                       {-# UNPACK #-} !ByteArray -- content
                                     
instance (Show a, Prim a) => Show (Matrix a) where
    show m@(Matrix nrows ncolumns _) = foldl (\z x -> z ++ show x ++ "\n") "" $ (map (\i -> (map (\j -> readMatrix m i j) [0..ncolumns-1]) ) [0..nrows-1])



data MMatrix s a = MMatrix {-# UNPACK #-} !Int -- number of rows
                           {-# UNPACK #-} !Int -- number of column  
                           {-# UNPACK #-} !(MutableByteArray s) -- content
                                     
thaw :: Matrix a  -> ST s (MMatrix s a)
thaw (Matrix r c a) = MMatrix r c <$> P.unsafeThawByteArray a

freeze :: MMatrix s a -> ST s (Matrix a)
freeze (MMatrix r c a) = Matrix r c <$> P.unsafeFreezeByteArray a



matrixFromList :: Prim a => Int -> Int -> [a] ->  Matrix a
matrixFromList nrows ncols list 
    | len == nrows*ncols = Matrix nrows ncols content
    | otherwise = error "The number of rows/cols does not fit the length of the data"

    where content = P.byteArrayFromList list
          len = length list



regA :: Int               
regA = 3

regB :: Int
regB = 4

for :: Monad m => Int -> Int -> (Int -> m ()) -> m ()
for n0 !n f = loop n0
  where
    loop i | i == n    = return ()
           | otherwise = f i >> loop (i+1)
    {-# INLINE loop #-}
{-# INLINE for #-}


kernel :: Matrix Double -> Matrix Double -> Matrix Double -> Matrix Double
kernel a b c = runST $ do
    mc <- thaw c
    for 0 regB $ \bi -> do

        let 
            bRow = readDoubleX4Matrix b 0 (bi*4) -- corresponds to bb, *4 since simd vec of width/step 4

        -- traceM . (++) "bRow:" . show $ DoubleX4 bRow

        for 0 regA $ \ai -> do

            let 
                aVar = unD (readMatrix a ai 0)
                aVec = broadcastDoubleX4# aVar
                resMul = timesDoubleX4# bRow aVec
            
            -- traceM . (++) "aVar:". show $ D# aVar

            -- traceM . (++) "aVec:". show $ DoubleX4 aVec

            -- traceM . (++) "resMul:". show $ DoubleX4 resMul

            cRowLifted <- C.primitive (readDoubleX4MMatrix mc ai (bi*4)) 



            let 
                cRow = unDoubleX4 cRowLifted
                cRes = plusDoubleX4# cRow resMul

            C.primitive_ $ writeDoubleX4MMatrix mc ai (bi*4) cRes -- used to be bi*4
    freeze mc
    



readMatrix :: (Prim a) => Matrix a -> Int -> Int -> a
readMatrix (Matrix _ ncols arr) i j = P.indexByteArray arr (i*ncols + j)
{-# INLINE readMatrix #-}

readMMatrix :: (Prim a) => MMatrix s a -> Int -> Int -> ST s a
readMMatrix (MMatrix _ ncols arr) i j = P.readByteArray arr (i*ncols + j)
{-# INLINE readMMatrix #-}

writeMMatrix :: (Prim a) => MMatrix s a -> Int -> Int -> a -> ST s ()
writeMMatrix (MMatrix _ ncols arr) i j val = P.writeByteArray arr (i*ncols + j) val
{-# INLINE writeMMatrix #-}

readDoubleX4Matrix :: Matrix a -> Int -> Int -> DoubleX4#
readDoubleX4Matrix (Matrix _ ncols (ByteArray arr)) i j = indexDoubleArrayAsDoubleX4# arr offset
    where offset = unI $ i*ncols + j
{-# INLINE readDoubleX4Matrix #-}

readDoubleX4MMatrix :: MMatrix s a -> Int -> Int -> State# s -> (# State# s, DoubleX4 #)
readDoubleX4MMatrix (MMatrix _ ncols (MutableByteArray arr)) i j s = let (# newS, vec #) = readDoubleArrayAsDoubleX4# arr offset s
                                                                     in (# newS, DoubleX4 vec #)  
    where offset = unI $ i*ncols + j
{-# INLINE readDoubleX4MMatrix #-}

writeDoubleX4MMatrix :: MMatrix s a -> Int -> Int -> DoubleX4# -> State# s -> State# s 
writeDoubleX4MMatrix (MMatrix _ ncols (MutableByteArray arr)) i j vec = writeDoubleArrayAsDoubleX4# arr offset vec
    where offset = unI $ i*ncols + j
{-# INLINE writeDoubleX4MMatrix #-}

unI :: Int -> Int#
unI (I# i) = i
{-# INLINE unI #-}

unD :: Double -> Double#
unD (D# d) = d
{-# INLINE unD #-}


