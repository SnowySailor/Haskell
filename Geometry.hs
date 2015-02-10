-- At the beginning, we specify the module name and the functions that it will export. 
-- It exports functions because when you import a module, you get the functions from it.
module Geometry (
	sphereVolume,
	sphereArea,
	cubeVolume,
	cubeArea,
	cuboidArea,
	cuboidVolume
) where
-- After the "where" is where we start writing our functions.

sphereVolume :: Float -> Float
sphereVolume radius = (4.0 / 3.0) * pi * (radius ^ 3)

sphereArea :: Float -> Float
sphereArea radius = 4 * pi * (radius ^ 2)

cubeVolume :: Float -> Float
cubeVolume side = cuboidVolume side side side 

cubeArea :: Float -> Float
cubeArea size = cuboidArea side side side