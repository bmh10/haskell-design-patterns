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

-- Foldable things can be expressed as lists by folding (:) over the Foldable:

main = do
  print $ F.toList aTree
  -- same as:
  print $ F.foldr (:) [] aTree

-- Example shows that not all folding accumulates destructively.
-- Fold accumulates the input structure into a single Monoid value, but the single value might be a composite structure.

-- Both map and filter are special cases of fold, making fold the fundamental function of structured recursion.

-- Foldable generalizes folding over arbitrary data structures, raising the concept of folding to type level.
-- Foldable generalizes structured recursion in Haskell from lists to container types.


-- Mapping over lists

-- Map is a specialization of fold since we can write map in terms of fold:

map f = foldr ((:).f) []

-- Just as with fold, we can map over lists with regular or monadic functions:

doF n = do print n; return (n * 2)
main = do
  print $ map (*2) [2, 3, 5, 7]
  mapM  doF [2, 3, 5, 7] >>= print
  mapM_ doF [2, 3, 5, 7]

--where:
map   :: (a -> b)   -> [a] -> [b]
mapM  :: (a -> m b) -> [a] -> m [b]
mapM  :: (a -> m b) -> [a] -> m ()

-- Previously we wrote mapM in terms of sequence and seqeucenA (the Monad and Applicative forms respectively).
-- When we use sequenceA, we get a function that maps over Applicative:

mapA :: Applicative f => (a -> f t) -> [a] -> f [t]
mapA f = sequenceA' . (map f)

sequenceA' :: Applicative f => [f t] -> f [t]
sequenceA' [] = pure []
sequenceA' (x:xs) = (:) <$> x <*> (sequenceA' xs)

main = mapA doF [2, 3, 5, 7] >>= print

-- This evaluates as:

--sequenceA' . (map doF) [2, 3, 5, 7]
-- (:) <$> (doF 2) <*> sequenceA' . (map doF) [3, 5, 7]
-- (4:) (:) <$> (doF 3) <*> sequence' . (map doF) [5, 7]
-- (4:6:10:14:[])

-- Even though evaluation is lazy, each list element is being visited twice:
-- The mapA instance traverses the list and applies f to each traversed list element.
-- sequenceA performs the resulting actions and re-assembles the results as a list

-- However, if we define mapA in the following way, we will have a single traversal:
mapA' f [] = pure []
mapA' f (x:xs) = (:) <$> f x <*> (mapA' f xs)

main = mapA' doF [2, 3, 5, 7] >>= print

-- Given mapA, we can define sequenceA in terms of it:
sequenceA = mapA id

-- This means that mapA and sequenceA can be defined interdependently:
mapA f = sequenceA . (map f)
sequenceA = mapA id

-- Appplicative map and sequence methods are at the heart of the Traversable type-class

-- Traversable

-- As with Prelude.foldM, mapM fails us beyond lists e.g. we cannot mapM over our Tree from earlier.
-- The Traversable type-class relates to map in the same manner as Foldable relates to fold:

class (Functor t, Foldable t) => Traversable (t :: * -> *) where
  -- Applicative form
  traverse  :: Applicative f => (a -> f b) -> t a -> f (t b)
  sequenceA :: Applicative f => t (f a) -> f (t a)

  -- Monadic form (redundant)
  mapM     :: Monad m => (a -> m b) -> t a -> m (t b)
  sequence :: Monad m => t (m a) -> m (t a)

-- traverse function generalizes our mapA function, which only works with lists, to work with all Traversable containers.
-- Similarly, Traversable.mapM is a more general version of Prelude.mapM for lists.

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM :: Monad m => (a -> m b) -> t a -> m (t b)

-- The Traversable type-class was introduced along with Applicative.

-- A Traversable Tree

-- Let's make our Tree an instance of Traversable. First the difficult way:

-- Traversable must also be a Functor and Foldable:
instance Functor Tree where
  fmap f (Leaf x) = Leaf (f x)
  fmap f (Node x lTree rTree) = Node (f x) (fmap f lTree) (fmap f rTree)

instance Foldable Tree where
  foldMap f (Leaf x) = f x
  foldMap f (Node x lTree rTree) = (foldMap f lTree) `mappend` (f x) `mappend` (foldMap f rTree)

-- traverse :: Applicative ma => (a -> ma b) -> mt a -> ma (mt b)
instance Traversable Tree where
  traverse g (Leaf x) = Leaf <$> (g x)
  traverse g (Node x ltree rtree) = Node <$> (g x) <*> (traverse g ltree) <*> (traverse g rtree)

data Tree a = Node a (Tree a) (Tree a) | Leaf a deriving (Show)

aTree = Node 2 (Leaf 3) (Node 5 (Leaf 7) (Leaf 11))

mainTraverse = traverse doF aTree
  where doF n = do print n; return (n * 2)

-- An easier way to do this is to auto-implement the functor, Foldable, and Traversable:

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
import Data.Traversable

data Tree a = Node a (Tree a) (Tree a) | Leaf a deriving (Show, Functor, Foldable, Traversable)

-- More details on this in chapter 6.

-- The Traversal and Iterator Pattern

-- Iterator pattern = "provide a way to access elements of an aggregate object sequentially without exposing its underlying representation"

-- Applicative traversal captures the iterator pattern.

-- The Traversable.traverse instance is the Applicative version of Traversable.mapM
-- Traversable.traverse is therefore more general than mapM (because Applicative is more general than Monad).

-- Also, because mapM does not rely on the Monad bind chain to communicate between iteration steps, Monad is a superfluous type for mapping with effects.
-- So traverse in Applicative is superior to the monadic traversal (mapM).

-- Modernizing Haskell 98

-- Introduction of Applicative, along with Foldable and Traversable, have had a big impact on Haskell.

-- Foldable and Traversable lift fold and map to a higher level of abstraction.

-- Traversable - describes a process that preserves the shape of the data structure being traversed over.
-- Foldable - discards or transforms the shape of the structure being folded over.

-- Since Traversable is a specialization of Foldable, we can say that shape preservation is a special case of shape transformation.
-- This distinction is clearly visible from the fact that function that discard their result (e.g. mapM_, forM_, sequence_, ...) are in Foldable, while their shape-preserving equivalents are in Traversable.

-- Applicable, Foldable and Traversable only recently found their way into Haskell.
-- Change way managed under the "Folable Traversable in Prelude" proposal which can be found online.
-- It includes the replacement of less generic functions in Prelude, Control.Monad, and Data.List with their more polymorphic counterparts in Foldable and Traversable.

-- There were some concerns about these changes as more generic types make the language harder to understand and learn.

-- Lenses

-- A Lens provides access to a particular part of a data structure.
-- They express a high-level pattern for composition but are also deeply entwined with Foldable and Traversable.
-- Lenses relate to getter/setter functions.

-- Deriving Lens

-- Tree from earlier:

data Tree a = Node a (Tree a) (Tree a) | Leaf a deriving (Show)

intTree = Node 2 (Leaf 3) (Node 5 (Leaf 7) (Leaf 11)
listTree = Node [1,1] (Leaf [2,1]) (Node [3,2] (Leaf [5,2]) (Leaf [7,4]))
tupleTree = Node (1,1) (Leaf (2,1)) (Node (3,2) (Leaf (5,2)) (Leaf (7,4)))

-- Generic getter/setter functions:

getRoot :: Tree a -> a
getRoot (Leaf z)     = z
getRoot (Node z _ _) = z

setRoot :: Tree a -> a -> Tree a
setRoot (Leaf z)     x = Leaf x
setRoot (Node z l r) x = Node x l r

mainLens1 = do
  print $ getRoot intTree
  print $ setRoot intTree 11
  print $ getRoot (setRoot intTree 11)

