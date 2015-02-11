module Geometry.Cuboid (
	area, 
	volume
) where

volume :: Float -> Float -> Float -> Float
volume a b c = a * b * c

area :: Float -> Float -> Float -> Float
area a b c = reactangleArea a b * 2 + reactangleArea b c * 2 + reactangleArea a c * 2

reactangleArea :: Float -> Float -> Float
reactangleArea a b = a*b