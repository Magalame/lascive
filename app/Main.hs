{-# LANGUAGE MagicHash #-}

module Main where

import Lib



main :: IO ()
main = do
    let m = 3
        n = 4*4 -- 4 avx registers, each can pack 4 doubles
        k = 2048*100
        
        a = matrixFromFunc m k $ (\i j -> 1)
        b = matrixFromFunc k n $ (\i j -> (fromIntegral (i*n + j)) :: Double)
        c = matrixFromFunc m n $ (\i j -> 0)

    -- print a

    let 
        d = {-# SCC "gemm" #-} gemm a b c

    print d