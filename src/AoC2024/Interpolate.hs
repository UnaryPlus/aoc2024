{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeOperators #-}
module AoC2024.Interpolate where

-- Fancy interpolation library

class InterpolateType a where
  interpolateAfter :: [String] -> String -> a

instance a ~ Char => InterpolateType [a] where
  interpolateAfter :: [String] -> String -> String
  interpolateAfter fore str = concat (reverse (str : fore))

instance a ~ () => InterpolateType (IO a) where
  interpolateAfter :: [String] -> String -> IO ()
  interpolateAfter fore str = putStrLn (interpolateAfter fore str)

instance (Show a, InterpolateType b) => InterpolateType (a -> b) where
  interpolateAfter :: [String] -> String -> a -> b
  interpolateAfter fore str x = 
    let (left, tail -> right) = break (== '$') str
    in interpolateAfter (show x : left : fore) right 

interpolate :: InterpolateType a => String -> a
interpolate = interpolateAfter []