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

-- This allows us to express + and * as a monoidal accumulation:

-- 10 + 7
(Sum 10) `mappend` (Sum 7) -- = Sum {getSum = 17}
(Product 10) `mappend` (Product 7) -- = Product {getProduct = 70}

-- Bool is a Monoid under the (||) and (&&) operators, corresponding to Any, All.
-- List instance is a Monoid under (++) and [].

-- So we can rewrite our earlier folds in monoidal form:

folds = do
  print $ mconcat [Sum 2, Sum 3, Sum 5]
  print $ mconcat [Product 2, Product 3, Product 5]
  print $ mconcat ["2", "3", "5"]
  print $ mconcat [["2"], ["3"], ["5"]]
  print $ mconcat [Any False, Any False, Any True]
  print $ mconcat [All True, All True, All True]

-- Folding accumulates and monoids are the datatype for accumulation.
-- This is why the generalization of fold, described by the Foldable library rests on Monoid.

-- Foldable

-- We can fold over lists uding foldl/r and monadic functions using foldM.
-- This doesn't help us when folding over other data structures e.g. trees

data Tree a = Node a (Tree a) (Tree a) | Leaf a deriving (Show)

-- If we constrain the inner type of Tree to be Monoid, we can fold over Tree:

foldT :: Monoid a => Tree a -> a
foldT (Leaf x) = x
foldT (Node x lTree rTree) = (foldT lTree) `mappend` x `mappend` (foldT rTree)

mainTreeSum = print . foldT $ Node (Sum 2) (Leaf (Sum 3)) (Leaf (Sum 5))

-- It's not ideal that we have to inject Sum/Product into the Tree structure.
-- We can avoid this by passing in a function to raise the elements being folded over to Monoid:

foldT' :: Monoid a => (t -> a) -> Tree t -> a
foldT' toMonoid (Leaf x) = toMonoid x
foldT' toMonoid (Node x lTree rTree)
 = (foldT' toMonoid lTree)
   `mappend` (toMonoid x)
   `mappend` (foldT' toMonoid rTree)

main = do
  print $ foldT' Sum aTree
  print $ foldT' Product aTree
  print $ foldT' (Any . (==5)) aTree
  print $ foldT' (All . (>0)) aTree
  where aTree = Node 2 (Leaf 3) (Leaf 5)

-- The Foldable type-class describes a more general interface for folding:

class Foldable (t :: * -> *) where
  foldMap :: Monoid m => (a -> m) -> t a -> m
  foldr :: (a -> b -> b) -> b -> t a -> b

-- foldMap function generalizes our foldT' instance:

foldT'  :: Monoid m => (a -> m) -> Tree a -> m
foldMap :: Monoid m => (a -> m) -> t a    -> m
fold    :: Monoid m =>             t m    -> m

-- fold assumes that the elements are already monoid:
fold = foldmap id

-- Let's make our tree a foldable instance:

import Data.Monoid
import qualified Data.Foldable as F
import qualified Control.Monad as M

instance F.Foldable Tree where
  foldMap toMonoid (Leaf x) = toMonoid x
  foldMap toMonoid (Node x lTree rTree)
    = (F.foldMap toMonoid lTree)
      `mappend` (toMonoid x)
      `mappend` (F.foldMap toMonoid rTree)

-- Instead of implementing fold for tree, we turned Tree into a Foldable container.

-- A Foldable instance comes with many convenience functions that generalize the corresponding Prelude functions:
main = do
  print $ F.sum aTree
  print $ F.product aTree
  print $ F.any (==5) aTree
  print $ F.all (>0) aTree
  print $ F.maximum aTree
  where aTree = Node 2 (Leaf 3) (Leaf 5)

-- Here we see how List is generalized to the Foldable container for sum:

sum :: Num a => [a] -> a
F.sum :: (Foldable t, Num a) => t a -> a

-- In the same way, Foldable.foldM generalizes folding over the Monad class:
foldM :: Monad m => (b -> a -> m b) -> b -> [a] -> m b

F.foldrM :: (Foldable t, Monad m) =>
  (a -> b -> m b) -> b -> t a -> m b

-- e.g.
doSum = F.foldrM doPlus
  where
    doPlus acc x = do
      putStrLn $ (show x) ++ " = " ++ (show acc)
      return (acc + x)

main = doSum 0 aTree


