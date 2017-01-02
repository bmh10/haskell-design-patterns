import System.IO
import Control.Monad
import Control.Applicative

-- Patterns for I/O

-- 3 styles: Imperative, Lazy, Iteratee

-- I/O as a first class citizen

-- IO monad provides context in which side effects can occur.
-- Allows decoupling of I/O code and pure code.

main = do
  h <- openFile "test.txt" ReadMode
  line <- hGetLine h
  putStrLn . show . words $ line
  hClose h

-- openFile and hGetLine are I/O actions that return values.
-- hClose and putStrLn are I/O actions that return nothing.
-- IO actions can be composed with pure functions e.g. putStrLn . show

-- 'do' is syntactic sugar for 'bind' (>>=)
-- hGetLine h >>= putStrLn . show . words

-- I/O as a functor, applicative, and monad

-- I/O monad is also an applicative functor, which in turn is a functor.
-- I/O as a functor allows us to use the fmap function e.g:
  line <- fmap (show . words) (hGetLine h)
  putStrLn line

-- I/O as an applicative functor means we can use this syntax instead of fmap:
  line <- (show . words) <$> (hGetLine h)
  putStrLn line

-- Monadic version:
  line <- liftM (show . words) (hGetLine h)
  putStrLn line

-- These 3 styles are all equivalent in this case.
-- Monad is more power than applicative, applicative is more powerful than functor.
-- Monad enables us to compose I/O actions together into seqeunced pipelines.
-- Applicative and functor allow us to apply functions to I/O actions.

-- Imperative I/O

