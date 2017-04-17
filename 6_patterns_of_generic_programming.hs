-- 6. Patterns of Generic Programming

-- In this chapter we seek a unified view of Generic Programming.
-- We start with a broad perspective by looking at Jeremy Gibbons' patterns of generic programming.
-- We then shift focus to a pattern: datatype-generic programming - which is characterized by generic functions parameterized by the shape of the datatype instead of the content.

-- We sample three basic approaches to datatype-generic programming: sum of products, origami programming, and scrapping your boilerplate.

-- We will also encounter a few exotic Haskell types - Typeable and Data; Bifunctor and Fix.
-- We will reveal the underpinning of Derivable type classes.
-- We will also discover the Gang of Four design patterns at the heart of datatype-generic programming.


-- Patterns of Generic Programming

-- Jeremy Gibbons describes seven patterns of generic programming, where each pattern is viewed as a different kind of parameterization.

-- Patterns 1 and 2 - functions

-- Functions parameterized by values are more general than functions with hardcoded values.
-- This is the simplest kind of generic programming.
-- Functions parameterized by functions (i.e. higher order functions) are a more powerful form of genericity.

-- Pattern 3 - polymorphic types and functions

-- Types parameterized by other types:

Tree a = Leaf a | Node a (Tree a)
-- is more generic than:
TreeI = Leaf Int | Node Int TreeI

-- Functions parameterized by polymorphic types:

f :: Tree a -> a
-- is more generic than:
g :: String -> String

-- Pattern 4 - type-class polymorphism

-- Previously we saw that the Foldable interface provides a uniform interface for folding over different types.
-- For a type to become Foldable it has to implement functions specific to the type.

-- The type class serves as a contract for ad hoc implementations. It also facilitates generic functions.
-- Through this combination of type-specific and generic functions, type classes give us a way to generalize functions over disparate types.
-- This can be referred to as ad hoc datatype genericity.


-- Pattern 5 - meta-programming

-- This pattern refers to program specified by other programs.
-- e.g. relection-based programming (analysing the structure of code and data), template-based meta-programming, or other code generation styles.

-- The Derivable type-classes

-- The Derivable type-classes enable code generation for type-class instances.
-- Haskell98 included autoderivation for the Eq, Ord, Enum, Bounded, Show, and Read type classes.

-- A second generation of Derivable type-classes was added to the GHC in stages for Data, Typeable, Functor, Foldable, Traversable, and Generic.

-- Haskell98 Derivable type-classes were achieved through compiler analysis of the structure of the derivable datatypes (i.e. a form of metaprogramming).
-- As there was no unifying formalism underlying the different types, the early Derivable type-classes represent only a rudimentary form of generic programming.

-- Generalized newtype deriving

-- The GeneralizedNewtypeDeriving language extension allows a newtype declaration to inherit some or all of the type-class instances of an inner type.
-- This is achieved through a trivial kind of meta-programming.
-- We used this extension when we created a Monad transformer stack:

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

newtype App a = App {runApp :: ReaderT Config (Writer String) a} deriving (Monad, MonadReader Config, MonadWriter String)

-- Pattern 6 - type laws

-- Many fundamental datatypes (e.g. Functor, Applicative, Arrow, Monad) are associated with mathematical laws that are meant to be obeyed by the type implementer.
-- Since the Haskell type system is not strong enough to express type laws in general, they are not enforceable by the compiler, so the implementer must take responsibility.

-- There are languages (e.g. Coq, Agda, Idris) with type systems designed for expressing laws and constraints for types.
-- Known as dependently-typed programming languages

-- This limitation in Haskell is a string motivator for Derivable type classes.
-- Generic implementations of type-classes still have to obey type laws.
-- We can ensure that the generic code is correct, so that implementers can assume that type laws are obeyed.

-- Pattern 7 - datatype generic programming

-- Datatype generic programming is a sophisticated pattern, based on a simple premise - instead of writing functions for ad hoc types, we deconstruct our types into a more fundamental type representation and then write generic functions against the lower-level type representation instead.

-- The lower level functions are then impervious to changes in the higher level datatypes.

-- Datatype-generic programming can be described as writing functions parameterized by the shape of the datatype.
-- It is said to exhibit shape/structure polymorphism or polytypism.

-- Parametric polymorphism abstracts from the occurrence of 'integer' is 'lists of integers'.
-- Datatype polymorphism abstracts from the occurrence of 'list'.

-- The reset of this chapters looks a key patterns of datatype-generic programming.


-- The Sum of Products Style

-- We use List and Tree for an example:

data List' a = Nil' | Cons' a (List' a) deriving (Show)
data Tree a = Node a (Tree a) (Tree a) | Leaf a deriving (Show)

aList = (Cons' 2 (Cons' 3 (Cons' 5 Nil')))
intTree = Node 2 (Leaf 3) (Node 5 (Leaf 7) (Leaf 11))

-- As a reference point, here are the datatype-specific size functions:

sizeT (Leaf _) = 1
sizeT (Node _ lt rt) = 1 + (sizeT lt) + (sizeT rt)

sizeL Nil' = 0
sizeL (Cons' _ xs) = 1 + (sizeL xs)

-- The shape of the functions follows the shape of the underlying recursive datatype.

-- Instead of using ad hoc polymorphic functions, let's write them in a datatype-generic way.
-- First we define a type representation.
-- We follow the style of Lightweight Implementation of Generics and Dynamics (LIGD).


-- The sum of products type representation

-- LIGD uses the sum of products style of type representation.
-- e.g. List' would  be deconstructed as either Nil or the combination of an element with another list.
-- In order to represent List' we need to be able to express 'Nil', 'choice of either', and 'combination of':

data List' a = Nil' | Cons' a (List' a)

-- Constructors that take no args (e.g. Nil') are represented as follows:

data U = U deriving (Show)

-- The simplify the example, we discard the constructor names and focus only on the structure of the datatype.
-- We encode the choice between multiple constructors in the style of the Either type:

data Choice a b = L a | R b deriving (Show)

-- List is a choice between U and Cons':

Choice U (Cons' a (List' a))

-- To represent the combo of 2 arguments of Cons', we use the following:

data Combo a b = Combo a b deriving (Show)

-- Consider:

Cons' a (List' a) -> Combo a (List' a)

-- This encodes type constructors with multiple args.
-- The List type can now be represented as:

type RList a = Choice U (Combo a (List' a))

-- The RList function does not recurse but refers to List' instead - this is called 'shallow type representation'.

-- Similarly we can represent Tree in this type representation:

type RTree a = Choice (Combo U a) (Combo a (Combo (Tree a) (Tree a)))

-- Combo and Choice both take 2 args but we can express multiple args through nesting:

(Combo a (Combo (Tree a) (Tree a)))

-- In the sum of products representation style, sum refers to Choice, product refers to Combo, and unit refers to U.


-- Translating between the type and representation

-- Now we have an alternative representation for List, we can translate to and from the type representation:

fromL :: List' a -> RList a
fromL Nil'         = L U
fromL (Cons' x xs) = R (Combo x xs)

toL :: RList a -> List' a
toL (L U)            = Nil'
toL (R (Combo x xs)) = (Cons' x xs)

main = do
  print $ fromL aList
  print $ (toL . fromList) aList

-- Let's capture the translation functions in one type:

data EP d r = EP {from :: (d -> r), to :: (r -> d)}


-- Writing a datatype-generic function

-- So far out type representation consists on the following 3 data types:

data U = U
data Choice a b = L a | R b
data Combo  a b = Combo a b

-- Since we want to define a generic function parameterized by this type representation, we need to group these dispare types into one.
-- There are different ways of donig this.
-- We will use a GADT:

data TypeRep t where
  RUnit :: TypeRep U
  RChoice :: TypeRep a -> TypeRep b -> TypeRep (Choice a b)
  RCombo  :: TypeRep a -> TypeRep b -> TypeRep (Combo a b)
  RInt :: TypeRep Int
  RType :: EP d r -> TypeRep r -> TypeRep d

-- TypeRep is a GADT because each constructor returns a different specialization of the general type TypeRep t.

-- Recall RList was defined in terms of the type representation datatypes:

type RList a = Choice U (Combo a (List' a))

-- We need a corresponding type based on the TypeRep constructors.
-- rList creates creates a more finely-typed representation that packages the list representation together with toL and fromL:

rList :: TypeRep a -> TypeRep (List' a)
rList tr = RType (EP fromL toL) (RChoice RUnit (RCombo tr (rList tr)))

-- rList is a recursive function using TypeRep constructors as building blocks.
-- The first arg (TypeRep a) guides the type resolution of List'.

-- This is why we need the RInt function:

rList (TypeRep Int) -- invalid
rList RInt          -- valid

-- We would need additional constructors RFloat, RDouble, RChar etc to deal with other types.

-- We can now write a generic function parameterized by both the type representation and the instance of the type:

gSize :: TypeRep a -> a -> Int
gSize RUnit U = 0
gSize (RChoice trA trB) (L a) = gSize trA a
gSize (RChoice trA trB) (R b) = gSize trB b
gSize (RCombo trA trB) (Combo a b) = (gSize trA a) + (gSize trB b)
gSize RInt _ = 1
gSize (RType ep tr) t = gSize tr (from ep t)

-- GADTs give a finer precision of types in data constructors, and therefore a finer precision when we perform pattern matching against algebraic datatypes.

-- Finally we can apply the gSize function to (List' Int):

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTS #-}

main = print $ gSize (rList RInt) aList

-- Adding a new datatype

-- Adding a new datatype, e.g. Tree, does not require amending the generic functions.
-- We already defined RTree and are left with fromT, toT, and rTree:

fromT :: Tree a -> RTree a
fromT (Leaf x) = L (Combo U x)
fromT (Node x lt rt) = R (Combo x (Combo lt rt))

toT :: RTree a -> Tree a
toT (L (Combo U x)) = Leaf x
toT (R (Combo x (Combo lt rt))) = (Node x lt rt)

rTree :: TypeRep a -> TypeRep (Tree a)
rTree tr = RType (EP fromT toT) (RChoice (RCombo RUnit tr) (RCombo tr (RCombo (rTree tr) (rTree tr))))

-- Now we use the datatype-generic gSize function on Tree:

main = print $ gSize (rTree RInt) intTree

-- We can now use the whole class of generic functions defined against the underlying type representation.
-- If we were to add gEq, gShow, gFold, gTraverse etc. They would all be automatically applicable to Tree and List.

-- GHC.Generics - a generic driving mechanism

-- A more generic approach was introduced in 2010 to synthesize the Derivable type-classes of Haskell 98.
-- The generic deriving mechanism was based on a more sophisticated extension of the sum of products approach.
-- The new approach also enabled autoderiving of user-defined type classes.
-- The GHC.Generics library includes this work.

-- Origami Programming

-- The the previous section we wrote a generic function for the recursive types Tree and List.
-- We now look at origami programming which focuses on the core patterns of recursion: map, fold, and unfold.

-- Tying the recursive knot

-- There is a primal type that underlies the recursive datatypes, known as Fix:

data List' a = Nil'   | Cons' a (List' a)
data Tree  a = Leaf a | Node  a (Tree a) (Tree a)

data Fix s a = FixT {getFix :: s a (Fix s a)}

-- s refers to the shape
-- a refers to an instance of the type

-- Fix is named after a fixed point of a function, which is defined by the following:

f (fix f) = fix f

-- To express Tree and List in terms of Fix, we need to rewrite them with an implicit recursion:

data List_ a r = Nil_    | Cons_ a r   deriving (Show)
data Tree_ a r = Leaf_ a | Node_ a r r deriving (Show)

-- We replaced the explicit recursive refs with a more vaguage parameter r.
-- We can now express ListF and TreeF in terms of Fix:

type ListF a = Fix List_ a
type TreeF a = Fix Tree_ a

-- The List_ and Tree_ functions don't explicitly recur, so Fix ties the recursive know around the shape.
-- We can construct a List_ function in a similar way:

-- aList1 :: List_ Integer (List_ a r)
aList1 = Cons_ 12 Nil_

-- aList2 :: Cons_ 12 (Cons_ 13 Nil_)
aList2 = Cons_ 12 (Cons_ 13 Nil_)

-- To construct the ListF lists, we need to wrap the FixT constructor around each nesting:
aListF :: ListF Integer
aListF = FixT (Cons_ 12 (FixT (Cons_ 13 (FixT Nil)))

-- The generic map

-- We deconstructed List and Tree into the generic recursion type Fix.
-- This means we can write generic functions against the Fix type.

-- Let's write a map for the fixed recursive type ListF:

mapL f listF = case list_ of
  (Cons_ x r) -> FixT $ Cons_ (f x) (mapL f r)
  Nil_        -> FixT Nil_
    where list_ = getFix listF

showListF :: (Show a) => ListF a -> String
showListF (FixT (Cons_ x r)) = (show x) ++ ", " ++ (showListF r)
showListF (FixT (Nil_)) = "Nil_"

mainGenericMap = putStrLn . showListF $ mapL (*2) aListF

-- This is clumsy because we have to unwrap the list with getFix and the rewrap the result with FixT.
-- Bifunctor provides sufficient flexibility to capture a wide variety of recursion patterns as datatype-generic programs.

-- Bifunctor is same as Functor except it can have 2 functions applied to it instead of 1.

-- from Data.Binfunctor
class Bifunctor s where
  bimap :: (a -> c) -> (b -> d) -> (s a b -> s c d)

-- Let's make List_ and Tree_ instances of Bifunctor:

instance Bifunctor List_ where
  bimap f g Nil_        = Nil_
  bimap f g (Cons_ x r) = Cons_ (f x) (g r)

instance Bifunctor Tree_ where
  bimap f g (Leaf_ x)       = Leaf_ (f x)
  bimap f g (Node_ x rl rr) = Node_ (f x) (g rl) (g rr)

-- Now we can write a generic map:
gmap :: Bifunctor s => (a -> b) -> Fix s a -> Fix s b
gmap f = FixT . bimap f (gmap f) . getFix

main = putStrLn . showListF $ gmap (*2) aListF

-- The generic fold

-- The bimap function also gives us a generic fold:

gfold :: Bifunctor s => (s a b -> b) -> Fix s a -> b
gfold f = f . bimap id (gfold f) . getFix

-- We unwrap the list with getFix but this time instead of unwrapping we apply f.
-- In other words, gfold replaces the occurrences of FixT with f:

-- FixT (Cons_ 12 (FixT (Cons_ 13 (FixT Nil_))))
-- f    (cons_ 12 (f    (Cons_ 13 (f    Nil_))))

-- To fold together a sum, we create an adder:

addL (Cons_ x r) = x + r
addL Nil_        = 0

mainAddL = print $ gfold addL aListF

-- Where fold is a consumer of data structures, unfold is a producer that unfolds a structure from a single value.
-- To unfold a regular list, we need a value and some functions:

unfoldL stopF nextF val 
  = if stopF val
    then []
    else val : (unfoldL stopF nextF (nextF val))

main = print $ unfoldL (< (-10)) (\x -> x - 1) 10

-- We can use bimap to create a generic unfold:

gunfold : Bifunctor s => (b -> s a b) -> b -> Fix s a
gunfold f = FixT . bimap id (gunfold f) . f

-- Consider the following example:

toList 0 = Nil_
toList n = (Cons_ n (n-1))

main = putStrLn . showListF $ gunfold toList 10

-- Generic unfold and fold

-- Composing unfold and fold makes sense because by doing so we are connecting a producer with a consumer:

main = print $ gfold addL (gunfold toList 100)

-- unfold and fold functions are mirror images:

gunfold f = FixT . bimap id (gunfold f) . f
gfold   f = f    . bimap id (gfold f)   . getFix

-- The hylo function is their composition:

hylo f g = g. bimap id (hylo f g) . f
main = print $ hylo toList addL 100

-- Origami programming is named due to its dependence on folds and unfolds.

-- Origami design patterns

-- 4 key Gand of Four design patterns are captured by origami recursioni operators:

-- Composite pattern: Recursive datatypes express the composite design pattern.
-- Since the recursive Fix captures a whole class of recursive datatypes, Fic captures the composite pattern most concisely.

-- Iterator pattern: An iterator gives linear access to the parts of a composite pattern in such a way that the shape of the traversed structure is preserved.
-- We can construct generic applicative traversals in the origami style we have seen here, capturing the iterator pattern.

-- Visitor pattern: earlier we saw this pattern was captured by polymorphic dispatch.
-- More generally, the visitor pattern is concerned with structured traversal of a composite - this is what fold enabled.
-- Visit like fold does not generally preserve the shape of the data structure being visited.
-- The iterator views the composite as a container of elements. The visitor views the composite shape as insignificant.

-- Builder pattern: separates the construction of a complex objects from its representation, so same construction process can create different representations.
-- We can express the structured construction of data structures with unfold.
-- The hylo function (unfold + fold) allows for data generation.

-- Scrap your boilerplate

-- SYB is another approach to datatype-generic programming. It provides a way to define generic functions over a universal type representation.
-- SYB differs from the other 2 approaches we explored in that the type representation is obfuscated from the user.

-- Here we create a generic traversal over a complex nested data structure:

data Book = Book Title [Chapter]
data Chapter = Chapter Title [Section]
data Section = Section Title [Para]
type Title = String
type Para = String

haskellDP = Book "Haskell DP" chapters
chapters = [Chapter "Chapter 1" sections1, Chapter "Chapter 2" sections2]
sections1 = [Section "1.1" ["S1"], Section "1.2" ["S1.2.1", "S1.2.2"]]
sections2 = [Section "2.1" ["S2.1"], Section "2.2" ["S2.2.1", "S2.2.2"]]

-- We have a function fSection which we want to apply to all functions in a Book.
-- Lens could do this as it can deliver a function to elements in a complex structure.
-- We use a different approach here:

fSection (Section t lines') = Section "!!!" lines'
main = print $ fSection (Section "1.1" ["S1"])

-- Our strategy will be to morph the function into one that can be applied to all parts of the data structure but will ignore elements for which it was not intended.


-- The type-safe cast with typeable

-- To make a type-safe cast possible, we will have to do a type comparison to check whether an element of a book is a section.

-- The Data.Typeable type-class has a cast function.
-- We can autoderive Typeable for our types:

{-# LANGUAGE DeriveDataTypeable #-}
import Data.Typeable

data Book = Book Title [Chapter] deriving (Show, Typeable)
data Chapter = Chapter Title [Section] deriving (Show, Typeable)
data Section = Section Title [Para] deriving (Show, Typeable)
type Title = String
type Para  = String

-- The cast function returns a Maybe value.
-- If type matches with type inside Maybe we get a Just value, otherwise we get Nothing.

-- Since functions are just types, we can cast them too.


-- Type-safe function application

-- We need a higher order fucntion to enable type-safe function application:

typesafeF :: (Typeable a, Typeable b) => (b -> b) -> a -> a

typesafeF f
 = case (cast f) of
        Just f' -> f'
        Nothing -> id

-- By lifting fSection into a type-safe function, we can apply it to any part of the Book type (or any Typeable type).

main = do
  print $ (typesafeF fSection) aSection
  print $ (typesafeF fSection) aBook
  where
    aSection = (Section "1.1" ["s1", "s2"])
    aBook = (Book "title" [])

-- typesafeF fSection leaves all values not targeted by the fSection function.


-- The shallow traversal and the data type-class


