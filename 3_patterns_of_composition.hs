-- Patterns Of Composition

-- We look at the composition of the fundamental type-classes: functor, applicative functor, arrow, monad.
-- Functor -> applicative functor -> arrow -> monad

-- Functor

-- Functor type-class gives us a way to generalize function application to arbitrary types.
-- Regular function:
f :: Num a => a -> a
f = (^2)

-- We can apply it directly to types if was intended for i.e. Numbers

-- To apply f to a richer type we need to make that type an instance of the Functor class and use fmap:

class Functor' f where
  fmap' :: (a -> b) -> f a -> f b

-- e.g. our own Maybe type:
data Maybe' a = Just' a | Nothing' deriving (Show)

instance Functor Maybe' where
  fmap _ Nothing'   = Nothing'
  fmap f (Just ' x) = Just' (f x)

-- By making Maybe' a Functor class we are describing how single-parameter functions may be applied to out type,
-- assuming the function types align with our Functor class:

-- we can do this:
a = fmap f (Just' 7)
b = fmap show (Just' 7)

-- but not this:
--c = fmap f (Just' "7")

-- fmaps lifts our function into the realm of the Functor.
-- It also lifts function composition to the level of functors.
-- Described by Functor laws:

