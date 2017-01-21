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

-- Law of Composition
-- fmap (f . g) == fmap f . fmap g

-- e.g. fmap (f . read) getLine == (fmap f) . (fmap read) $ getLine

-- Identity Law
-- fmap id == id
-- e.g. fmap id (Just 1) = id (Just 1)

-- fmap is to Functor what map is to the List type:

ns  = map  (^2) [1,2,3]
ns' = fmap (^2) [1,2,3]

-- This works because List if a Functor:

-- instance Functor List where
--   fmap = map

-- The Functor class abstracts the idea of function application to a single argument.
-- It gives us a way of combining functions with types by lifting a function from one level of abstraction to another.


-- Applicative Functor

-- Because Maybe is a Functor we can lift the (+2) function so that it can be applied directly to a Maybe value:
-- e.g. fmap (+2) (Just 3)

-- However fmap does not allow us to apply a function to multiple Functor values i.e. fmap (+) (Just 2) (Just 3)

-- For this we need the Applicative Functor class. It enabled us to raise a function to act on multiple Functor values:
-- Applicative inherits from Functor
class (Functor f) => Applicative f where
  pure  :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

-- pure function lifts a value into the Functor class
-- <*> operator generalizes function application to the Functor class (hence 'applicative functor')

-- Lets make Maybe' an instance of Applicative:

data Maybe' a = Just' a | Nothing' deriving (Show)

instance Functor Maybe' where
fmap _ Nothing'  = Nothing'
fmap f (Just' x) = Just' (f x)

instance Applicative Maybe' where
  pure f = Just' f
  Nothing'  <*> _         = Nothing'
  _         <*> Nothing'  = Nothing'
  (Just' f) <*> (Just' x) = Just' (f x)

--e.g:

pure (,) <*> Just' 2 <*> Just' 3
-- evals as:
Just' (,) <*> Just' 2 <*> Just' 3
Just' ((,) 2)         <*> Just' 3
Just' ((,) 2 3)
Just' (2,3)


-- Currying of the lower-level function (,) leads to currying on the Applicative level.
-- Function composition on the lower level is also preserved at the applicative level.

-- The law of composition of Applicative Functor:
