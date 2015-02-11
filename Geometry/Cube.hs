module Geometry.Cube (
	area,
	volume
) where

import qualified Geometry.Cuboid as GC

volume :: Float -> Float
volume side = GC.volume side side side

area :: Float -> Float
area side = GC.area side side side