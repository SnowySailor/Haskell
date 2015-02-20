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
-- How about a function that nudgs a shape?
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


-- RECORD SYNTAX --

-- If we want to make a person, we could define it as
-- data Person = Person String String Int Float String String deriving (Show)
-- But then we would have to define a lot of different functions to get a specific value, and the placement of the differnet types doesn't make
-- it apparent what value goes where.
-- Instead, we can define it like this
data Person = Person { 
	firstName :: String,
	lastName :: String,
	age :: Int,
	height :: Float,
	phoneNumber :: String,
	flavor :: String
} deriving (Show)
-- Now, Haskell already made functions for firstName, lastName, etc. to return the values.

-- There's also another benefit to using record syntax. If we just define a Car as
-- data Car = Car String String Int deriving (Show)
-- Then when we try to show a car, it only prints out "Car "Ford" "Mustang" 1967"
-- If we define a car as 
data Car = Car {
	company :: String,
	model :: String,
	year :: Int
} deriving (Show)
-- Then we show a car
doCar :: Car
doCar = Car {company = "Ford", model = "Mustang", year = 1967}

-- It will display like this:
-- Car {company = "Ford", model = "Mustang", year = 1967}
-- We also don't have to put the specific names in a certain order because we are assigning using a key-value-ish system.


-- TYPE PARAMETERS --

-- A value constuctor can take a few parameters and return a result. As an example, Car takes three values and produces a car value. 
-- Type constuctors can also take values just like value constuctors to create new types. 
-- data Maybe a = Nothing | Just a
-- Here, the a is the type parameter. Because there is a type parameter involved, we call this a type constuctor. Depending on what
-- we want this type constuctor to hold, it can produce Maybe Int, Maybe Car, etc. No value will have a type of just Maybe because it's not
-- exactly a type. It's a type constuctor. In order for it to be a type, it needs to have all of its parameters filled up.
-- So if we pass in a Char to Maybe, we get Maybe Char. The type of "Just 'a'" is Maybe Char.
-- We used a type that has a type parameter before Maybe. Lists have a type paramater. [Int], [Char], [[String]]. However you can't just have []

-- Type parameters are useful because ti allows us to make different types based on what you want contained. If the a in Just a is a String,
-- then the a in Maybe a is also a String.

-- The type of Nothing is Maybe a because its type is polymorphic. If some funciton requires Maybe Int, we can give it Nothing because Nothing 
-- doesn't contain any values anyway, so it doesn't matter. The type Maybe a can act as Maybe Int or Maybe Double just like 5 can act as
-- an Int or Double. An empty list can act as a list of anything. That's why we can do [1,2,3] ++ [].

-- Using type parameters is very beneficial, but only when using them makes sense. For example, we wouldn't define Car as 
-- {company :: a, model :: b, year :: c} because company and model will always be a String and the year will always be an Int. If our type
-- acts as some kind of a box, it's good to use them. 
-- Here's a funciton to show the properties of a car.
tellCar :: Car -> String
tellCar (Car {company = c, model = m, year = y}) = "This " ++ c ++ " " ++ m ++ " was made in " ++ show y

-- This function would have to be made if the car was declared with types a, b, and c.
-- tellCar' :: (Show a) => Car String String a -> String
-- tellCar' (Car {company = c, model = m, year = y}) = "This " ++ c ++ " " ++ m ++ " was made in " ++ show y
-- As you can see the type declaration is very gross looking and only benefits us in that we can use any data type that is of the Show typeclass

-- So we would rather just use Car String String Int to make it simple. We usually use type parameters when the type inside of the value doesn't
-- really need to be a certian one for the type to work. A list of stuff is just a list of stuff. If we want it to be a list of integers, 
-- we can specify that in the function's type declaration. The same goes for Maybe values. Maybe represents having either nothing or have one
-- of something. It doesn't matter what the type of that thing is. 

-- Another example of a parameterized type that we've met is Map k v from Data.Map. The k is the key in the map, and the v is the value. This
-- is an example of where type parameters are useful. This allows us to have two types, but it doesn't matter what types they actually are. 
-- So you can have a key that's an Int and a value that's a String. Or the other way around. Having maps parameterized allows us to have
-- mappings from any type to any other type as long as they key is within the Ord typeclass. 

-- data (Ord k) => Map k v = ...
-- It's a very strong convention to never add typeclass constraints in data declarations because we don't benefit a lot. We just end up writing 
-- more class constraints. If we do have the Ord k in the data delcaration, then we won't have to write Ord k when writing a function that uses
-- Map, but if we don't put it in, we don't need to have Ord k in our function type delcaration. 
-- An example of this is toList. It's defined like toList :: Map k v -> [(k,v)]
-- If we had Ord k in our data declaration, we would need to write toList :: (Ord k) => Map k v -> [(k,v)] even though the function does nothing
-- with comparing by order.
-- So don't put typeclass constraints into data declarations even if it makes sense at the time because you'll have to put them into the
-- function declarations either way. 

-- Here'd a 3D vector type
data Vector a = Vector a a a deriving (Show)
-- Function to add two vectors.
vplus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector a b c) `vplus` (Vector d e f) = Vector (a+d) (b+e) (c+f)

-- Function to multiply two vectors together
vmult :: (Num a) => Vector a -> Vector a -> Vector a
(Vector a b c) `vmult` (Vector d e f) = Vector (a*d) (b*e) (c*f)

-- Function to add together the products of the scalar values of two vectors
scalarMult :: (Num a) => Vector a -> Vector a -> a
(Vector a b c) `scalarMult` (Vector d e f) = (a*d) + (b*e) + (c*f)
-- Notice how we didn't put the Num a constraint in the data declaration because we would need to do the same thing in the funciton declaration.