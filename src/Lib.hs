{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Lib
    where

import Data.Primitive (Prim, ByteArray(..), MutableByteArray(..))
import qualified Data.Primitive as P

import qualified Control.Monad.Primitive as C
import Control.Monad.Primitive (PrimMonad, PrimState) 

import qualified GHC.Prim as G
import GHC.Prim (DoubleX4#, Double#, Int#, State#)

import GHC.Base (Int(..), Double(..))
import Control.Monad.ST (ST, runST)

import Control.DeepSeq (NFData,rnf)

import Debug.Trace



data DoubleX4 = DoubleX4 DoubleX4#

instance Show DoubleX4 where
    show (DoubleX4 vec) = "(" ++ show (D# a) ++ "," ++ show (D# b) ++ "," ++ show (D# c) ++ "," ++ show (D# d) ++ ")"
        where (# a, b, c, d #) = G.unpackDoubleX4# vec

unDoubleX4 :: DoubleX4 -> DoubleX4#
unDoubleX4 (DoubleX4 vec) = vec



data Matrix a = Matrix {-# UNPACK #-} !Int -- number of rows
                       {-# UNPACK #-} !Int -- number of column  
                       {-# UNPACK #-} !ByteArray -- content
    deriving ()
                                     
instance (Show a, Prim a) => Show (Matrix a) where
    show m@(Matrix nrows ncolumns _) = foldl (\z x -> z ++ show x ++ "\n") "" $ (map (\i -> (map (\j -> readMatrix m i j) [0..ncolumns-1]) ) [0..nrows-1])

instance (NFData a) => NFData (Matrix a) where
    rnf (Matrix r c arr) = seq r . seq c . seq arr $ ()

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


simdW :: Int
simdW = 4

regsA :: Int               
regsA = 3

mr :: Int 
mr = regsA

regsB :: Int
regsB = 4

nr :: Int -- width of the row of B we'll load during kernel
nr = regsB*simdW

kc :: Int -- height of the L1 cache
kc = 2048

mc :: Int -- height of the L2 cache
mc = 8

nc :: Int -- width of the L3 cache
nc = 48



for :: Monad m => Int -> Int -> (Int -> m ()) -> m ()
for n0 !n f = loop n0
  where
    loop i | i == n    = return ()
            | otherwise = f i >> loop (i+1)
{-# INLINE for #-}

for_step :: Monad m => Int -> Int -> Int -> (Int -> m ()) -> m ()
for_step n0 !n step f = loop n0
  where
    loop i | i >= n    = return ()
           | otherwise = f i >> loop (i+step)
{-# INLINE for_step #-}

-- traceM . (++) "bRow:" . show $ DoubleX4 bRow

gemm :: Matrix Double -> Matrix Double -> Matrix Double -> Matrix Double
gemm a@(Matrix m k _) b@(Matrix _ n _) c = runST $ do
    mutc <- thaw c

    for_step 0 n nc $ \jc -> do -- nc = L3/kc*sizePrimitive

        for_step 0 k kc $ \pc -> do -- kc = L1 size / regsB*simdRegisterSize, 256 for avx2

            for_step 0 m mc $ \ic -> do -- mc = L2/kc*sizePrimitive

                for_step 0 (min nc n) nr $ \jr -> do -- nr is regsB*simdW

                    for_step 0 (min mc m) mr $ \ir -> do -- mr is regsA 

                        for 0 (min kc k) $ \p -> do -- kc = L1 size / regsB*simdRegisterSize, 256 for avx2

                            kernel a b mutc (ir + ic) (p + pc) (jr + jc) 

    freeze mutc
    


kernel :: (PrimMonad m) => Matrix Double -> Matrix Double -> MMatrix (PrimState m) Double -> Int -> Int -> Int -> m ()
kernel a b c !i !k !j = do

    -- traceM $ show i ++ "," ++ show k ++ "," ++ show j

    for_step 0 nr simdW $ \bi -> do

        let 
            bRow = readDoubleX4Matrix b k (j+bi) -- corresponds to bb, *4 since simd vec of width/step 4

        for 0 regsA $ \ai -> do

            let 
                aVar = unD (readMatrix a (i+ai) k)
                aVec = G.broadcastDoubleX4# aVar
                resMul = G.timesDoubleX4# bRow aVec
            
            cRowLifted <- C.primitive (readDoubleX4MMatrix c (i+ai) (j+bi)) 

            let 
                cRow = unDoubleX4 cRowLifted
                cRes = G.plusDoubleX4# cRow resMul

            C.primitive_ $ writeDoubleX4MMatrix c (i+ai) (j+bi) cRes -- used to be bi*4
{-# INLINE kernel #-}

readMatrix :: (Prim a) => Matrix a -> Int -> Int -> a
readMatrix (Matrix _ ncols arr) !i !j = P.indexByteArray arr (i*ncols + j)
{-# INLINE readMatrix #-}

readMMatrix :: (Prim a) => MMatrix s a -> Int -> Int -> ST s a
readMMatrix (MMatrix _ ncols arr) !i !j = P.readByteArray arr (i*ncols + j)
{-# INLINE readMMatrix #-}

writeMMatrix :: (Prim a) => MMatrix s a -> Int -> Int -> a -> ST s ()
writeMMatrix (MMatrix _ ncols arr) !i !j !val = P.writeByteArray arr (i*ncols + j) val
{-# INLINE writeMMatrix #-}

readDoubleX4Matrix :: Matrix a -> Int -> Int -> DoubleX4#
readDoubleX4Matrix (Matrix _ ncols (ByteArray arr)) !i !j = G.indexDoubleArrayAsDoubleX4# arr offset
    where !offset = unI $ i*ncols + j
{-# INLINE readDoubleX4Matrix #-}

readDoubleX4MMatrix :: MMatrix s a -> Int -> Int -> State# s -> (# State# s, DoubleX4 #)
readDoubleX4MMatrix (MMatrix _ ncols (MutableByteArray arr)) !i !j s = let (# newS, vec #) = G.readDoubleArrayAsDoubleX4# arr offset s
                                                                     in (# newS, DoubleX4 vec #)  
    where !offset = unI $ i*ncols + j
{-# INLINE readDoubleX4MMatrix #-}

writeDoubleX4MMatrix :: MMatrix s a -> Int -> Int -> DoubleX4# -> State# s -> State# s 
writeDoubleX4MMatrix (MMatrix _ ncols (MutableByteArray arr)) !i !j !vec = G.writeDoubleArrayAsDoubleX4# arr offset vec
    where !offset = unI $ i*ncols + j
{-# INLINE writeDoubleX4MMatrix #-}

unI :: Int -> Int#
unI (I# i) = i
{-# INLINE unI #-}

unD :: Double -> Double#
unD (D# d) = d
{-# INLINE unD #-}


