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


