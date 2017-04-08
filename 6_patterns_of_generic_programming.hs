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




