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

obj_f2 :: ObjU a -> String
obj_f2 (Obj v f2 _) = f2 v

mainObjU = do
  print $ obj_f1 obj -- even 3
  print $ obj_f2 obj -- show 3
    where obj = (ObjU 3 even show)

-- We've packed a value with some functions in the ObjU object but we haven't encapsulated the value as a true object would have.
-- This is what existential quantification enables us to do.

-- Existential quantification and abstract datatypes

-- We now introduce an existentially quantified ObjE object which hides the type parameter instead of the universally qualified ObjU object.
-- The type param is no longer present on the left-hand side:

data ObjE = forall a . ObjE a (a -> Bool) (a -> String)

-- This means that the type parameter is also hidden in the type signature of objE_f1/2:

objE_f1 :: ObjE -> Bool
objE_f1 (ObjE v f1 _) = f1 v

objE_f2 :: ObjE -> String
objE_f2 (ObjE v f2 _) = f2 v

-- requires {-# LANGUAGE ExistentialQuantification #-}
mainObjE = do
  print $ objE_f1 obj -- even 3
  print $ objE_f2 obj -- show 3
    where obj = (ObjE even show)

-- We can access an encapsulated object property only with the functions packaged with that property.
-- e.g. we cannot apply any other function to the following property:

-- INVALID (cannot infer types)
objE_f3 (ObjE v f1 f2) = v

-- Existential quantification provides us with the means to implement abstract datatypes, thus providing functions over a type while hiding the representation of the type.
-- We can also achieve the abstract datatypes on a higher level by using Haskell's module system to hide algebraic datatype constructors while exporting functions over the type.

-- From now on we will refer to types that are existentially qualified as 'existentials'.

-- Universal                   | Existential
-- Type parameterization       | Type abstraction
-- Parametric polymorphism     | Encapsulation
-- User of data specifies type | implementer of data specified type
-- forall = "for all"          | forall = "for some"

-- Phantom Types

-- Phantom types were introduced in 1999 as a solution to the challenges that arise when embedding a type-safe domain specific language (DSL) in Haskell.

-- Consider this trivial expression language and evaluator:

data Expr1 = I1 Int | Add Expr1 Expr1

eval1 :: Expr1 -> Int
eval1 (I1 v) = v
eval1 (Add1 x y) = (eval1 x) + (eval1 y)

-- When we add another base type (B2 Bool) to the expression language, the situation becomes more complicated:

data Expr2 = I2 Int | B2 Bool | Add2 Expr2 Expr2 deriving Show

-- This has brought about 2 problems:
-- 1. Add2 was only meant to work with I2 Ints, but we can now construct a bad value. The regular algebraic datatypes don't allow us to express this relationship constraint between 2 constructors:

-- construct a 'bad' value
(Add2 (I2 11) (B2 True))

-- 2. The type inference can no longer be inferred or defined for eval:

-- INVALID
eval2 :: Expr2 -> t
eval2 (I2 v) = v
eval2 (B2 v) = v
eval2 (Add2 x y) = (eval2 x) + (eval2 y)

-- In this case phantom types solve the first problem by adding a type t in:

data Expr3 t = I3 Int | B3 Bool | Add3 (Expr3 Int) (Expr3 Int) deriving Show

-- The type t serves as a type placeholder that can be used by each constructor to describe its particular type.
-- However, all the constructors still return the same type:

I3   :: Int  -> Expr3 t
B3   :: Bool -> Expr3 t
Add3 :: Expr3 Int -> Expr3 Int -> Expr3 t

-- The Expr3 value is parametrized by the type t, but t does not appear in any of the constructors, hence the term 'phantom type'.
-- We can still construct invalid values:

Add3 (I3 11)  (B3 True)

-- However, we can still use the phantom type information to create type-safe smart constructors:

i3 :: Int -> Expr3 Int
i3 = I3

b3 :: Bool -> Expr3 Bool
b3 = B3

add3 :: Expr3 Int -> Expr3 Int -> Expr3 Int
add3 = Add3

-- If we use the smart constructors instead of the datatype constructors, the Haskell type-checker will prevent us from creating invalid values.
-- For example the following will be rejected:

-- INVALID
add3 (i3 10) (b3 True)

-- However, type inference remains a problem because the values are still not described accurately:

(I3 12) :: Expr3 t -- not :: Expr3 Int

-- The effect is that adding values remains too ambiguous:

eval3 (Add3 x y) = (eval3 x) + (eval3 y)

-- Despite these limitations, phantom types containue to be useful in other areas.
-- e.g. the Lens library uses the const phantom type to great effect.

-- We have seen how Phantom types enable type-safe construction (problem '1' from earlier).
-- In order to solve problem '2' we need generalized algebraic datatypes (GADTs).

-- Generalized Algebraic Datatypes

-- GADTs emerged independently from both ML and Haskell camps in 2003 and were part of GHC by 2005.
-- GADTs bring together phantom types, smart constructors, and refined pattern matching:

{-# LANGUAGE GADTs #-}

data Expr t
  where
    -- Built-in smart constructors
    I :: Int  -> Expr Int
    B :: Bool -> Expr Bool
    Add :: Expr Int -> Expr Int -> Expr Int

-- The GADTs smart constructors describe constrained instances of Expr t.
-- As with phantom types, smart constructors provide increased type safety for data construction.
-- However, GADT's give us something we don't get from phantom types:

eval :: Expr t -> t
eval (I v) = v
eval (B v) = v
eval (Add x y) = (eval x) + (eval y)

-- eval (Add (I 10) (I 12))

-- This is because GADT smart constructors are built into the type and we can match the pattern on them.
-- This solves the problem of type inference the we had with phantom types.
-- This is why GADTs are also known as first-class phantom types.

-- GADTs are not expressed by syntax but rather by the relationship between the type parameters and the constructor return types.
-- Similarly, phantom types are not expressed by syntax but implied by the lack of appearance of a type parameter in the type constructors.

-- There is a subtle drift in the meaning of type parameter, from signifying the type of some embedded value to expressing the type metadata.


-- Typecase Pattern

-- Generic programming is another important use case for GADTs.
-- As an example, consider a type representation Rep that unifies the 3 variable types Int, Char, and List:

data Rep t where
  RInt  :: Rep Int
  RChar :: Rep Char
  RList :: Show a => Rep a -> Rep [a]

-- The RList function can be thought of as being existentially qualified ('a' does not appear on the LHS).
-- The phantom t in Rep t will serve as the type metadata.

-- We can now write a function that takes a value along with its type representation:

showT :: Show t => Rep t -> t -> String
showT RInt i  = (show i) ++ " :: INT"
showT RChar i = (show i) ++ " :: Char"

showT (RList rep) [] = "THE END"
showT (RList rep) (x:xs) = (showT rep x) ++ ", " ++ (showT (RList rep) xs)

-- The showT function is a type-indexed function because it is defined for each member of the family of types Rep t:

showT RInt 3
showT (RList RInt)  [12, 13, 14]
showT (RList RChar) ['2','3','5']

-- To be more precise, showT is a closed type-indexed function because the type index family (Rep t) is fixed.

-- In contrast, the show function of the Show type-class is an example of an open type-indexed function.
-- show is simply type-indexed by instances of Show and considered 'open' because we can add new types to the type index freely.

-- In languages that allow us to reflect on the type of a value, we can write showT by dealing with the parameter value on a "type-case" basis:

-- pseudo code
case (type t) of
  Int  -> (show t) ++ ":: INT"
  Char -> (show t) ++ ":: CHAR"
  List -> -- show list

-- This style has been distilled into a design pattern:
-- TypeCase -> a design pattern that allows the definition of closed type-indexed functions, in which the index family is fixed but the collection of functions is extensible.

-- Dynamic Types


