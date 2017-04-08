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
