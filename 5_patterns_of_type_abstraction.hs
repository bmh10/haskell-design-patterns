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

-- In this section we describe a series of patterns related to data abstraction.
-- We start with existentially quantified types, then move to phantom types and end with GADTs.

-- Universal Quantification

-- Let's explore existential quantification from the perspective of its opposite, universal quantification.
-- We rely on universal quntification whenever we parameterize function types. e.g:

id' :: a -> a
-- Is the same as:
id' :: forall a. a -> a
id' x = x

-- In general, universal quantification expresses parametric polymorphism in functions and datatypes.
-- We use the forall keyword in the Rank-n function type to indicate nested parametric polymorphism.
-- Similarly, universal quantification is the default pattern when parameterizing types with types:

data Maybe' a = Nothing' | Just' a

-- conceptually (but not practically) the same as:
data Maybe' a = forall a. Nothing' | Just' a

-- As another example, consider the following universally quantified type:

data ObjU a = ObjU a       -- property
             (a -> Bool)   -- obj method
             (a -> String) -- obj method

-- Here we mimic an object with the property of type a and 2 object methods.
-- We can apply a method to the property by extracting the value and the method with pattern matching:

obj_f1 :: ObjU a -> Bool
obj_f1 (Obj v f1 _) = f1 v

obj_f2 :: ObjU a -> Bool
obj_f2 (Obj v f2 _) = f2 v

main = do
  print $ obj_f1 obj -- even 3
  print $ obj_f2 obj -- show 3
    where obj = (ObjU 3 even show)

-- We've packed a value with some functions in the ObjU object but we haven't encapsulated the value as a true object would have.
-- This is what existential quantification enables us to do.

-- Existential quantification and abstract datatypes


