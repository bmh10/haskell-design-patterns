-- 1. Functional Patterns - the building blocks

--Gluing modules together in haskell:
--1. Higher-order functions
--2. Currying
--3. Recursion
--4. Types, Pattern matching, polymorphism
--5. Lazy evaluation
--6. Monads

-- Functions as first class citizens
sq = \x -> x * x

-- Currying functions
greetCurried :: String -> String -> String
greetCurried title name = "Hi " ++ title ++ " " ++ name

greetUncurried :: (String, String) -> String
greetUncurried (title, name) = "Hi " ++ title ++ " " ++ name

g n = (n^2, n^3)
res = uncurry max (g 11)

-- Curried functions are composable, uncurried functions are not

-- Decoupling with currying
-- Partial function application can be used for decoupling

