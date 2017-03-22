-- Patterns Of Type Abstraction

-- In this chapter we look at some of the key advances on one of the major fronts of Haskell's evolution: language extensions.

-- Haskell extensions are tied to the compiler implementation rather than the language standard.
-- We'll explore the extensions along three axes: abstracting functions, datatypes, and type-classes.

-- 1. Abstracting function types: RankNTypes
-- 2. Abstracting datatypes: Existential quantification, phantom types, generalized algebraic datatypes (GADTs), type case pattern, dynamic types, and heterogeneous lists.
-- 3. Abstracing type-classes: Multiparameter type-classes and functional dependencies.

-- Abstracting function types: RankNTypes

-- Consider the higher order function that maps the argument function to each tuple element:

tupleF elemF (x, y) (elemF x, elemF y)

-- Left to its own devices, the Haskell 98 compiler will infer this type for a tupleF function:

tupleF :: (a -> b) -> (a, a) -> (b, b)

-- As elemF is applied to x and y, the compiler assumes that x and y must be of the same type, hence the inferred tuple type (a, a).
-- This allows us to do the following:

tupleF length ([1, 2, 3], [3, 2, 1])
tupleF show (1, 2)
tupleF show (True, False)

-- But not this:

tupleF show (True, 2)
tupleF length ([True, False, False], [1, 2, 4])

-- RankNTypes allow us to enforce parametric polymorphism explicitly.
-- We want tupleF to accept a ploymorphic function of arguments; in other words, we want our function to have a 'higher rank type', in this case Rank 2:

{-~ LANGUAGE Rank2Types #-}

tupleF' :: (Show a1, Show a2) => (forall a . Show a => a -> b) -> (a1, a2) -> (b, b)
tupleF' elemF (x, y) = (elemF x, elemF y)

-- The use of forall in the elemF function signature tells the complier to make elemF polymorphic in a, as shown:

mainRankN1 = do
  -- same as before
  print $ tupleF' show (1, 2)
  print $ tupleF' show (True, False)

  -- and now we can do this
  print $ tupleF' show (True, 2) -- polymorphic

-- The rank of the type refers to the nesting depth at which polymorphism occurs.
-- Rank 0 describes the absence of polymorphism. e.g:

intAdd :: Int -> Int -> Int

-- Rank 1 refers to regular parametric polymorphism:
id :: a -> a

-- tupleF' is Rank 2.
-- Rank n describes deeper nested polymorphism.
-- Typically the cases for using deeper levels of polymorphism are rare, however when you need higher-rank types there are usually no workarounds.


-- Abstracting Datatypes

