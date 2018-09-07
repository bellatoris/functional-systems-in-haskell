x = 2
y = 3

main = let z = x + y        -- let introduces local binding
       in print z           -- program will print 5

add arg1 arg2 = arg1 + arg2
five = add 2 3

main2 = print (add 2 3)

safeDiv x y = 
    let q = div x y
    in if y == 0 then 0 else q

main3 = print (safeDiv 3 0)

-- main4 = 
--     let x = x + 1
--     in print x
--

factorial :: (Ord p, Num p) => p -> p
factorial n = 
    if n > 1
    then n * factorial (n - 1)
    else 1

factorial2 n =
    let loop acc n' = if n' > 1
                      then loop (acc * n') (n' - 1)
                      else acc
    in loop 1 n

factorial3 n = let loop acc n' | n' > 1 = loop (acc * n') (n' - 1)
                               | otherwise = acc
               in loop 1 n

-- | symbol introduces a guard
-- Guard are evaluated top to bottom; the first True guard wins
-- The system Prelude (standard library) defines otherwise = True

factorial4 n = loop 1 n
    where loop acc n'   | n' > 1 = loop (acc * n') (n' - 1)
                        | otherwise = acc
-- Unlike let, a where clause scopes over multiple guarded definitions
-- This is conveinient for binding variables to use in guards

xy :: Integer
xy = (1 :: Integer) + (1 :: Integer) :: Integer

add2 :: Integer -> (Integer -> Integer)
add2 arg1 arg2 = arg1 + arg2

data PointT = PointC Double Double deriving Show
-- Declares new type. PointT with constructor PointC containing two Doubles
-- deriving show means you can pring the type (helpful in GHCI)
-- Can also derive Read, Eq, Ord, Enum, Bounded

-- Note that types and constructor must start with Capital letters
-- Types and construcotr can use the same name (often do), E.g.:
data Point = Point Double Double deriving Show

-- One type can have multiple constructor (like a tagged union):
data Point2 = Cartesian Double Double
            | Polar Double Double
              deriving Show

data Color = Red | Green | Blue | Violet deriving (Show, Eq, Enum)
