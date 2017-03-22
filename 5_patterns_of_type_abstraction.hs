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

  
