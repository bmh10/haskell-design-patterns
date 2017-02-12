-- Patterns of Folding and Traversing

-- With the introduction of Applicative came more powerful mapping (traversal), which enabled type-level folding and mapping.
-- First we look at how the Prelude's list fold is generalized to all Foldable containers.
-- Then we follow the generalization of the list map to all Traversable containers.
-- We then look at the Lens library which raises Foldable and Traversable to an even higher level of abstraction.

-- Folding over lists

-- Recall from Chapter 1:

sumLazy []     = 0
sumLazy (x:xs) = x + sumLazy xs

-- This is captured by the recursive process foldr:

sumLazy' = foldr (+) 0

-- We can also write a strict sum function:
sumStrict acc []     = acc
sumStrict acc (x:xs) = sumStrict (acc + x) xs

-- This is captured by foldl:

sumStrict' = foldl (+)

-- The foldl function is tail recursive and strict in the accumulator.
-- Foldr is non-tail recursive and lazy.
-- Foldl is a special case of foldr.

-- Folding with monadic functions

doSumStrict :: (Show a, Num a) => a -> [a] -> IO a

doSumStrict acc [] = return acc
doSumStrict acc (x:xs) = do
  putStrLn $ " + " ++ (show x) ++ " = " ++ (show acc')
  doSumStrict acc' xs
  where acc' = acc + x

main = doSumStrict 0 [2, 3, 5, 7]

-- To write as a left-fold, we use the foldM function:
doSumStrict' = foldM doPlus
  where doPlus acc x = do
    putStrLn $ " + " ++ (show x) ++ " = " ++ (show acc)
    return (acc + x)

main = doSumStrict' 0 [2, 3, 5, 7]

-- Both foldl/r and foldM describe folding over lists:
foldM :: Monad m => (b -> a -> m b) -> b -> [a] -> m b
foldl ::            (b -> a -> b)   -> b -> [a] -> b

-- For foldM, the fold function is monadic and the result accumulates inside the Monad class.

-- Folding with Monoids

-- Folding allows us to express all manner of accumulations:

sum'     = foldr (+) 0
product' = foldr (*) 1
concatS' = foldr (++) ""
concatL' = foldr (++) []

any' = foldr (||) False
all' = foldr (&&) True

main = do
  print $ sum' [2, 3, 5]
  print $ product' [2, 3, 5]
  print $ concatS' ["2", "3", "5"]
  print $ concatL' [["2", "3", "5"]]
  print $ any'     [False, False, True]
  print $ all'     [True, True, True]

-- These accumulation functions differ only in the same function and initial value.
-- In each case, the initial value is also the identity for the corresponding operator:
0 + x = x
1 * x = x
"" ++ x = x
[] ++ x = x
False || x = x
True && x = x

-- The monoid instance describes exactly this - an associative operator with an identity value:

class Monoid a where
  mempty :: a
  mappend :: a -> a -> a
  mconcat :: [a] -> a
  mconcat = foldr mappend mempty

-- mconcat is a generic version of our accumulation functions.

-- Numbers are monoidal under addition and multiplication:

newtype Sum' a = Sum' {getSum' a} deriving (Show)
instance Num a => Monoid (Sum' a) where
  mempty = Sum' 0
  Sum' x `mappend` Sum' y = Sum' (x + y)

newtype Product' a = Product' {getProduct' :: a}
instance Num a => Monoid (Product' a) where
  mempty = Product' 1
  Product' x `mappend` Product' y = Product' (x * y)

-- These are already defined in Data.Monoid as Sum and Product
