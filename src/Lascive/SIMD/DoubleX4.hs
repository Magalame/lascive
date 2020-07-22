{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE CPP #-}

module Lascive.SIMD.DoubleX4 (

    DoubleX4(..)
    
    ) where

import Data.Primitive (Prim)
import qualified Data.Primitive as P

import qualified GHC.Prim as G
import GHC.Prim (DoubleX4#, Double#, Int#)

import Data.Primitive.MachDeps  (sIZEOF_DOUBLE, aLIGNMENT_DOUBLE)

import GHC.Base (Int(..), Double(..))






unI :: Int -> Int#
unI (I# i) = i
{-# INLINE unI #-}


data DoubleX4 = DoubleX4 DoubleX4#

instance Show DoubleX4 where
    show (DoubleX4 vec) = "(" ++ show (D# a) ++ "," ++ show (D# b) ++ "," ++ show (D# c) ++ "," ++ show (D# d) ++ ")"
        where (# a, b, c, d #) = G.unpackDoubleX4# vec



mul4 :: Int# -> Int#
mul4 i# = unI (I# i# * 4)

#define derivePrim(ty, sz, align, idx_arr, rd_arr, wr_arr, set_arr, idx_addr, rd_addr, wr_addr, set_addr) \
instance Prim ty where {                                        \
  sizeOf# _ = unI sz                                           \
; alignment# _ = unI align                                     \
; indexByteArray# arr# i# = ty (idx_arr arr# (mul4 i#))               \
; readByteArray#  arr# i# s# = case rd_arr arr# (mul4 i#) s# of        \
                        { (# s1#, x# #) -> (# s1#, ty x# #) }  \
; writeByteArray# arr# i# (ty x#) s# = wr_arr arr# (mul4 i#) x# s#    \
; indexOffAddr# addr# i# = ty (idx_addr addr# (mul4 i#))              \
; readOffAddr#  addr# i# s# = case rd_addr addr# (mul4 i#) s# of       \
                        { (# s1#, x# #) -> (# s1#, ty x# #) }  \
; writeOffAddr# addr# i# (ty x#) s# = wr_addr addr# (mul4 i#) x# s#   \
; {-# INLINE sizeOf# #-}                                        \
; {-# INLINE alignment# #-}                                     \
; {-# INLINE indexByteArray# #-}                                \
; {-# INLINE readByteArray# #-}                                 \
; {-# INLINE writeByteArray# #-}                                \
; {-# INLINE indexOffAddr# #-}                                  \
; {-# INLINE readOffAddr# #-}                                   \
; {-# INLINE writeOffAddr# #-}                                  \
}
          
derivePrim(DoubleX4, (sIZEOF_DOUBLE*4), (aLIGNMENT_DOUBLE*4),
           G.indexDoubleArrayAsDoubleX4#, G.readDoubleArrayAsDoubleX4#, G.writeDoubleArrayAsDoubleX4#, G.setDoubleArray#,
           G.indexDoubleOffAddrAsDoubleX4#, G.readDoubleOffAddrAsDoubleX4#, G.writeDoubleOffAddrAsDoubleX4#, G.setDoubleOffAddrAsDoubleX4#)
