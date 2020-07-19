{-# LANGUAGE MagicHash #-}

module Main where

import Lib



main :: IO ()
main = do
    let m = 3
        n = 4*4 -- 4 avx registers, each can pack 4 doubles
        k = 2048*10
        
        a = matrixFromList m k $ take (m*k) $ repeat (1 :: Double)
        b = matrixFromList k n $ [0..(4*4*2048*10)-1 :: Double]
        c = matrixFromList m n $ take (m*n) $ repeat (0 :: Double)

    let 
        d = gemm a b c

    print d