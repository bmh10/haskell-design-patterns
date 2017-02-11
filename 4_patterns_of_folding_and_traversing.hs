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
