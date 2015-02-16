-- MAKING OUR OWN TYPES AND TYPECLASSES --
module Shapes (
	Point(..), 
	Shape(..),
	surface,
	nudge,
	baseCircle,
	baseRectangle
) where -- Export types like [TYPE]([Constructors]|..)
	
-- ALGEBRAIC DATA TYPES INTRO --
data Bool = False | True
-- We use 'data' to say that we're defining a new data type. The part before the = is the name of the type, and the parts after are the value
-- constructors. They're the different values the type can have. 
-- We define data Int = -2147483648 | -2147483647 | ... | -1 | 0 | 1 | 2 | ... | 2147483647 (it doesn't actually have the ... ).

-- How can we represent a shape in Haskell? We can use a tuple (x-cord, y-cord, radius), but that only works for circles, and it's not exactly
-- very specific about what value is what. This could also represent a 3-D vector or a bunch of other things. It's better to use a bunch of 
-- different things to represent a shape. 

-- data Shape = Rectangle Float Float Float Float | Circle Float Float Float deriving (Show) -- OLD --
data Point = Point Float Float deriving (Show)  
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)  

-- The Shape type is made up of two different constructors (Rectangle and Circle). Rectangle takes four Floats, the first two are coordinates 
-- of the lower left corner and the last two are coordinates of the upper right corner. With the Cirle, the first two are the coordinates of
-- the center, and the last is the radius.

-- Constructors are functions like everything else. 
-- An important thing to see is the type declaration here. We take a SHAPE. Not a Circle or a Rectangle. The only thing that's a type is Shape.
-- Just like we can't write a function with True -> Int
surface :: Shape -> Float
-- surface (Circle _ _ r) = pi * r ^ 2 -- OLD --
-- surface (Rectangle x1 y1 x2 y2) = (abs $ x1 - x2) * (abs $ y1 - y2)
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x1 - x2) * (abs $ y1 - y2)
surface (Circle _ r) = pi * r ^ 2


-- We can also pattern match against constructors. Because we aren't worried about the positioning of the Circle, we only need the radius.
-- If we just try to give Haskell "Circle 5 5 10", we get an error because it doesn't know how to display that on the screen.
-- We can add 'deriving ([TYPE])' to the data declaration in order to tell Haskell that it can be shown.

-- Since value constructors are functions, we can map them and partially apply them.
mapFunction :: Float -> Float -> [Shape]
mapFunction a b = map (Circle (Point 5.0 4.0)) [a,b,5,6,7]

-- We have a good Shape data type, but we can make it better by implimenting a Point data type (line 14)

-- How about a function taht nidges a shape?
nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))

-- If we don't want to have to deal directly with points, we can define a function that wil create a base circle or a base rectangle and then
-- just nudge that.
baseCircle :: Float -> Shape
baseCircle r = Circle (Point 0 0) r
baseRectangle :: Float -> Float -> Shape
baseRectangle width height = Rectangle (Point 0 0) (Point width height)
-- Now we can just nudge in order to declare the new position without having to type out Point all the time. 