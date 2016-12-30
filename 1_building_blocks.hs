import System.Random

-- 1. Functional Patterns - the building blocks

--Gluing modules together in haskell:
--1. Higher-order functions
--2. Currying
--3. Recursion
--4. Types, Pattern matching, polymorphism
--5. Lazy evaluation
--6. Monads

-- Functions as first class citizens
sq = \x -> x * x

-- Currying functions
greetCurried :: String -> String -> String
greetCurried title name = "Hi " ++ title ++ " " ++ name

greetUncurried :: (String, String) -> String
greetUncurried (title, name) = "Hi " ++ title ++ " " ++ name

g n = (n^2, n^3)
res = uncurry max (g 11)

-- Curried functions are composable, uncurried functions are not

-- Decoupling with currying
-- Partial function application can be used for decoupling

-- Recursion
-- Functions and types can be recursive

-- Not-tail recursion
sumNonTail [] = 0
sumNonTail (x:xs) = x + (sumNonTail xs)
-- This is not tail-recursive because the recursion is 'trapped' by the + operator.
-- This requires whole list to be stored in memory for the sum to be performed.

-- Tail recursion
sumTail' acc [] = acc
sumTail' acc (x:xs) = sumTail' (acc + x) xs
sumTail xs = sumTail' 0 xs
-- This uses constant space and is therefor more efficient.

--sumNonTail is a recursive function that expresses a recursive process.
--sumTail is a recursive function that expresses an iterative process.

-- Folding abstracts recursion

-- foldl expands the same as sumTail'
foldlSum = foldl (+) 0

-- foldr expands the same as sumNonTail
foldrSum = foldr (+) 0

-- foldl/foldr def:
foldr' _ v [] = v
foldr' f v (x:xs) = f x (foldr' f v xs)
-- Recursion is 'trapped' by f, making foldr non-tail recursive

foldl' _ v [] = v
foldl' f v (x:xs) = foldl' f (f v x) xs

-- Types, pattern matching, and polymorphism

data Persion = P String Int         -- combination -> product of types
data MaybeInt = NoInt | JustInt Int -- alternation -> sum of types
data Maybe' a = Nothing' | Just' a  -- generic

-- Algebraic data type constructors serve as 'deconstructors' in pattern matching.
-- Left of '=' -> deconstruct, right of '=' -> construct
-- Patern matching is therefore the complement of algebraic data types.
fMaybe f (Just' x) = Just' (f x)
fMaybe f Nothing'  = Nothing'

-- Recursive types
-- Composite pattern can be captured using recursive algebraic types
data Tree a = Leaf a | Branch (Tree a) (Tree a)

-- Functions over recursive types are typically also recursive:
size :: Tree a -> Int
size (Leaf _) = 1
size (Branch l r) = size l + size r + 1

-- Polymorphism
-- Type 1 : Parametric

length' :: [a] -> Int
length' []     = 0
length' (x:xs) = 1 + length xs

-- Above function exhibits parametric polymorphism because it acts uniformly
-- on a range of types with common structure (i.e. lists).
-- Functions defined on parametric data types tend to be generic.

-- Type 2 : Ad-hoc
-- Also known as 'overloading'

-- Standard example is the polymorphic + operator:
{-
class Num a where
  (+) :: a -> a -> a

instance Int Num where
  (+) :: Int -> Int -> Int
  x + y = intPlus x y

instance Float Num where
  (+) :: Float -> Float -> Float
  x + y = floatPlus x y
-}

-- In an expression x = 1 + 3.14. Since 1 is of type Num it can be treated
-- as an int, float etc. depending on context.

-- Alternation based ad-hoc polymorphism

data Shape = Circle Float | Rect Float Float

area :: Shape -> Float
area (Circle r) = pi * r^2
area (Rect l w) = l * w

-- Area function is dispatched over the alternations of the Shape type

-- Class based ad-hoc polymorphism

data Circle = Circle Float
data Rect = Rect Float Float

class Shape a where
  area :: a -> Float

instance Shape Circle where
  area (Circle r) = pi * r^2

instance Shape Rect where 
  area (Rect l w) = l * w

-- Here the two classes are unified through a Shape class.


-- Polymorphic dispatch and the visitor pattern

-- Static type dispatch -> resolved at compile time
-- Dynamic type dispatch -> resolved at runtime

-- Double dispatch (2 args) example:

data CustomerEvent = InvoicePaid Float | InvoiceNonPayment
data Customer      = Individual Int    | Organisation Int

payment_handler :: CustomerEvent -> Customer -> String

payment_handler (InvoicePaid amt) (Individual id)   = "SendReceipt for " ++ (show amt)
payment_handler (InvoicePaid amt) (Organisation id) = "SendReceipt for " ++ (show amt)

payment_handler (InvoiceNonPayment) (Individual id)   = "CancelService for " ++ (show id)
payment_handler (InvoiceNonPayment) (Organisation id) = "SendWarning for " ++ (show id)

-- payment_handler defined behaviour for all 4 permutations.
-- In OOP we have to resort to the visitor pattern to achieve multiple dispatch.

-- Unifying parametric and ad-hoc polymorphism

-- Parametric polymorphism -> single generic functions acts on variety of types (true polymorphism)
-- Ad-hoc polymorphism     -> overloaded funtion that is resolved to particular function by compiler (apparent/synthetic polymorphism)

-- Therefore:
-- Parametric polymorphism -> allows us to lift the level of abstraction
-- Ad-hoc polymorphism     -> gives us powerful tool for decoupling

-- Haskell blurs the distinction e.g.

{-
class Eq a where
  (==), (/=) :: a -> a -> Bool
  x == y = not (x /= y)
  x /= y = not (x == y)

== and /= are given mutually recursive definitions. Implementer has to implement at least one.
Therefore, once function would be specialised (ad-hoc), leaving the other to be defiend at a
generic level (parametric).
-}

-- Functions, types and patterns

-- Strategy Pattern

strategy fSetup fTeardown
  = do
      fSetup
      -- carry out main function purpose
      fTeardown

-- Caller passes in functions as arguments. Allows decoupling an algorithm from parts that may change.


-- Template Pattern

-- Strategy pattern uses delgation to vary an algorithm
-- Template pattern uses inheritance to vary an algorithm -> in Haskell we can use type classes to achieve the same:

class TemplateAlgorithm where
  setup :: IO a -> a
  teardown :: IO a -> a
  doWork :: a -> a
  fullfillPurpose
    = do
        setup
        work
        teardown


-- Iterator Pattern

it = map square [2, 3, 5, 7]

-- map navigates list strcuture, while square acts on individual elements.
-- Therefore control flow is decoupled from function application, as with the Iterator Pattern


-- Decoupling behaviour and modularizing code

-- Passing one function into another => decoupling two parts of code.
-- The functions can be changed at different rates and reside in different modules.

-- Lazy Evaluation

-- Cons (:) is lazy. It only evaluates its first argument.
-- All functions in Haskell are lazy by default.
-- One of the motivations of the Proxy Pattern is to defer evaluation => this is done implicity by lazy evaluation.

-- Streams

-- Self-reference ex:
infinite42s = 42 : infinite42s

takeFromInfiniteEx = (take 5 infinite42s)

-- Example of random number generation:

generate :: StdGen -> (Int, StdGen)
generate g = random g :: (Int, StdGen)

mainRandomEx = do
  gen0 <- getStdGen
  let (int1, gen1) = generate gen0
  let (int2, gen2) = generate gen1

-- Each time we want a new random int we must pass the new generator to generate function.

-- We can hide this to make our intent more clear by using an infinite stream:

randInts' g = (randInt, g) : (randInts' nextGen)
  where (randInt, nextGen) = generate g

-- Select only the random ints (hide the generators):
randInts g = map fst (randInts' g)

mainRandomEx2 = do
  g <- getStdGen
  print $ take 3 $ randInts g

-- Stream of rands between 0 - 100:
randAmounts g = map (\x -> x `mod` 100) (randInts g)

-- Consuming random numbers is now independent from producing them.
-- Also, have decoupling between iteration and termination.

-- Modeling change with streams
