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

