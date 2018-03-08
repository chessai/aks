{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}

{-# OPTIONS_GHC -Wall #-}

module Prime
  ( prime
  ) where

import GHC.Int
import GHC.Exts

prime :: Int -> Bool
prime !p
  | p < 2     = False
  | otherwise = go p 1 1

go :: Int -> Int -> Int -> Bool
go !p !z !i = let !z' = z * (p - i + 1) `unsafeQuot` i in
  if i < p - 1
    then if unsafeRem z' p == 0 || i < 2
      then go p z' (i + 1)
      else False
    else True

unsafeQuot :: Int -> Int -> Int
unsafeQuot (I# i) (I# j) = I# (quotInt# i j)

unsafeRem :: Int -> Int -> Int
unsafeRem (I# i) (I# j) = I# (remInt# i j)
