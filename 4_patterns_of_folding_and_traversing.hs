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

-- If we want to pass in a setter function instead of a value:

fmapRoot :: (a -> a) -> Tree a -> Tree a
fmapRoot f tree = setRoot tree newRoot
  where newRoot = f (getRoot tree)

-- We get the root, apply the function, then set the result.
-- This double work is akin to the double traversal we saw when writing traverse in terms of sequenceA.
-- We resolved the issue by defining traverse.

-- We can use the same approach here by writing fmapRoot to work in a single step:

fmapRoot' :: (a -> a) -> Tree a -> Tree a
fmapRoot' f (Leaf z)     = Leaf (f z)
fmapRoot' f (Node z l r) = Node (f z) l r

setRoot' :: Tree a -> a -> Tree a
setRoot' tree x = fmapRoot' (\_ -> x) tree

main = do
  print $ setRoot' intTree 11
  print $ fmapRoot' (*2) intTree

-- fmapRoot' function delivers a function to a particular part of the structure and returns the same structure.
-- To allow IO, we need a new function:
  fmapRootIO :: (a -> IO a) -> Tree a -> IO (Tree a)

-- We can generalize this beyond IO to all monads:

fmapM :: (a -> m a) -> Tree a -> m (Tree a)

-- If we relax the requirement for Monad and generalize to all Functor container types f',
-- we get a simple van Larrhoven Lens of type:

type Lens' s a = Functor f' => (a -> f' a) -> s -> f' s

-- The good thing about a van Laarhoven Lens is that, given the above function type, we also gain get, set, fmap, and mapM,
-- along with many other functions and operators.

-- The Lens function type signature is all it takes to make something a Lens that can be used with the Lens library.
-- It is unusual to use a type signature as the 'primary interface' for a library.
-- The immediate benefit is that we can define a Lens without referring to the Lens library.


-- Writing a Lens

-- A Lens provides focus on an element in a data structure.

-- Our first Lens will focus on the root node of a Tree instance.
-- Using the Lens type signature as a guide, we arrive at the following:

lens' :: Functor f  => (a -> f' a) -> s      -> f' s
root  :: Functor f' => (a -> f' a) -> Tree a -> f' (Tree a)

-- This is not very tangible. fmapRootIO is easier to understand, with the Functor f' being IO:

fmapRootIO :: (a -> IO a) -> Tree a -> IO (Tree a)
fmapRootIO g (Leaf z)     = (g z) >>= return . Leaf
fmapRootIO g (Node z l r) = (g z) >>= return . (\x -> Node x l r)

displayM x = print x >> return x

mainLens2 = fmapRootIO displayM intTreei

-- If we drop down from Monad into Functor, we have a Lens for the root of a Tree:

root :: Functor f' => (a -> f' a) -> Tree a -> f' (Tree a)
root g (Node z l r) = fmap (\x -> Node x l r) (g z)
root g (Leaf z)     = fmap Leaf (g z)

-- As monad is a functor, this function also works with monadic functions:

mainLens3 = root displayM intTree

-- As root is a Lens, the Lens library gives us the following:

-- import Control.Lens
main = do
  -- GET
  print $ view root listTree
  print $ view root intTree
  -- SET
  print $ set root [42] listTree
  print $ set root 42 intTree
  -- FMAP
  print $ over root (+11) intTree

-- The over instance is the way in which Lens uses fmap to map a function into a Functor method.


-- Composable getters and setters

-- We can have another Lens on a Tree instance which focuses on the rightmost leaf:

rightMost :: Functor f' => (a -> f' a) -> Tree a -> f' (Tree a)

rightMost g (Node z l r) = fmap (\r' -> Node z l r') (rightMost g r)
rightMost g (Leaf z)     = fmap (\x -> Leaf x) (g z)

-- The Lens library provides several lenses for Tuple (e.g. _1 which brings focus to the first Tuple element).
-- We can compose our rightMost lens with the Tuple lenses:

mainLens4 = do
  print $ view rightMost       tupleTree
  print $ set  rightMost (0,0) tupleTree

  -- Compose getters and setters
  print $ view (rightMost._1)        tupleTree
  print $ set  (rightMost._1) 0      tupleTree
  print $ over (rightMost._1) (*100) tupleTree

-- A Lens can serve as a getter, setter, or modifier.

-- We compose Lenses using regular function composition (.)
-- Note that the order of composition is reversed i.e. in (rightMost._1), the rightMost lens is applied before the _1 lens.

-- Lens Traversal

-- A Lens focuses on one part of a data structure, not several; e.g. a Lens cannot focus on all the leaves of a Tree.

-- To focus on more than one part of a structure, we need a Traversal, the Lens generalization of Traversable.
-- As Lens relies on Functor, Traversal relies on Applicative.
-- Other than this, the signatures are exactly the same:

traversal :: Applicative f' => (a -> f' a) -> Tree a -> f' (Tree a)
lens      :: Functor f'     => (a -> f' a) -> Tree a -> f' (Tree a)

-- A leaves Traversal delivers the setter function to all the leaves of the Tree:

leaves :: Applicative f' => (a -> f' a) -> Tree a -> f' (Tree a)
leaves g (Node z l r) = Node z <$> leaves g l <*> leaves g r
leaves g (Leaf z) = Leaf <$> (g z)

-- We can use set and over with our new Traversal instance:

set leaves 0 intTree
over leaves (+1) intTree

-- Traversals compose seamlessly with Lenses:

main = do
  -- Compose traversal + Lens
  print $ over (leaves._1) (*100) tupleTree

  -- Compose Traversal + Traversal
  print $ over (leaves.both) (*100) tupleTree

  -- map over each elem in target container
  print $ over (leaves.mapped) (*(-1)) listTree

  -- Traversal with effects
  mapMOf leaves displayM tupleTree

-- The both function is a Tuple Traversal that focuses on both elements.


-- Lens.Fold

-- The Lens.Traversal function lifts Traversable into the realm of lenses, while Lens.Fold does the same for Foldable:

main = do
  print $ sumOf leaves intTree
  print $ anyOf leaves (>0) intTree

-- Just as for Foldable sum and foldMap, we can write Lens sumOf in terms of foldMapOf:

getSum $ foldMapOf lens Sum

-- where foldMapOf is a generalization of Foldable.foldMap

-- The Lens Library

-- We've only uses simple Lenses; a full parameterized Lens would allow for replacing parts of a data structure with different types:

type Lens s t a b = Functor f' => (a -> f' b) -> s -> f' t
type Lens' s a = Lens s s a a -- Simple Lens

-- Lens library function names do their best not to clash with existing names e.g. droppingWhile vs dropWhile
-- This typically allows for an unqualified import of the Lens library.

-- By leaving the Lens function type transparent we get Traversals by simply swapping out functor for Applicative in the Lens type definition.
-- We also get to define lenses without having to reference the Lens library.
-- One downside is that Lens type signatures can be bewildering at first:

mapMOf :: Profunctor p => Over p (WrappedMonad m) s t a b -> p a (m b) -> s -> m t
foldMapOf :: Profunctor p => Accessing p r s a -> p a r -> s -> r

-- On the surface, the Lens library gives us composable getters and setters, but there is much more to the Lens library than that.
-- By generalizing Foldable and Traversable into Lens abstractions, the Lens library lifts getters, setters, lenses, and Traversals into
-- a unified framework in which they call compose together.

-- Lens library has been critized for not relecting idiomatic Haskell and for taking on too much responsibility.

-- Summary

-- We followed the evolution of fold and map, starting with lists, then generalizing for all Foldable/Traversable containers.
-- We then saw how the Lens library places folding and traversing in an even broader context.
-- Lenses give us a unified vocabulary to navigate through data structures -> "query language for data structures"
-- As we moved up through the layers of abstraction, the function type signatures became ever more generic.
