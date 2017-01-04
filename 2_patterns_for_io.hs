import System.IO
import Control.Monad
import Control.Applicative
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as LB8
import Data.Char (chr)

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

-- Haskell I/O code is purely functional but the style can be imperative:

data Chunk = Chunk {chunk :: String} | LineEnd {chunk :: String, remainder :: String} deriving (Show)

mainImperative = do
  fileH <- openFile "test.txt" ReadMode
  loop "" fileH
  hClose fileH
  where
    loop acc h = do
      isEof <- hIsEOF h
    if isEof
      then do putStrLn acc; putStrLn "DONE..."
      else do
        chunk <- B.hGet h 8
        case (parseChunk chunk) of
          (Chunk chunk') -> do
            let accLine = acc ++ chunk'
            loop accLine h
          (LineEnd chunk' remainder) -> do
            let line = acc ++ chunk'
            -- Process line...
            putStrLn line
            loop remainder h

parseChunk chunk
  = if rightS == B8.pack ""
      then Chunk   (toS leftS)
      else LineEnd (toS leftS) ((toS . B8.tail) rightS)
  where
    (leftS, rightS) = B8.break (== '\n') chunk
    toS = map (chr . fromEnum) . B.unpack

-- Imperative I/O is also known as 'handle-based I/O'

-- Pros:
-- Processing is incremental.
-- Precise control over resources (e.g. open/close files, start long-running processes).

-- Cons:
-- I/O expressed at low level of abstraction.
-- Produces code which is not very composable. e.g. above iteration over the file is interleaved with chunk processing.
-- The traversal state is exposed. We pass the file handle around, check for EOF on each iteration, and must explicity clean up the resource.

-- Lazy I/O

-- Of the 3 main 'glues' in Haskell - High Order Functions (HOFs), the type system, and laziness - laziness is the only one which is
-- not a concrete thing in the language (cannot be seen in the code). Rather it is related to the way the code will be evaluated at runtime.

mainLazy = do
  -- Lazy IO stream
  let ios = map putStrLn ["this", "won't", "run"];
  putStrLn "until ios is seqeunced..."
  sequence_ ios -- performs actions

-- This discards action results because the (>>) operator discards the results:
sequence_ :: [IO ()] -> IO ()
sequence_ = foldr (>>) (return ())

-- The sequence function retains the results:
mainLazy2 = do
  h <- openFile "test.txt" ReadMode
  line1 <- hGetLine h
  let getLines = [hGetLine h, hGetLine h]
  [line2, line3] <- sequence getLines
  hClose h
  putStrLn line2

-- line1 is read eagerly. line2/3 are only read when sequenced.

-- hGetLine returns a strict string, wheras hGetContents returns a lazy string:
mainLazy3 = do
  h <- openFile "test.txt" ReadMode
  contents <- hGetContents h
  putStrLn (take 10 contents) -- Lazily fetch 10 chars
  hClose h

-- Using the lines function with hGetContents, we get a lazy stream of file lines:
lineStream h = hGetContents h >>= return . lines

mainLazy4 = do
  h <- openFile "test.txt" ReadMode
  lines' <- lineStream h
  mapM_ putStrLn lines' -- mapM_ f arg = sequence_ (map f arg)
  hClose h

-- mapM captures the common pattern of mapping and sequencing.

-- forM is same as mapM but with reversed arguments. This is useful for passing a 'trailing lambda' function:
mainLazy5 = do
  h <- openFile "test.txt" ReadMode
  lines' <- lineStream h
  forM_ lines' $ \line -> do
        let rev = reverse line
        putStrLn rev
  hClose h

-- When performing I/O we must make the distinction between I/O actions and performing an I/O action.
-- Also need to know the lazy/strict characteristic of the functions we are working with e.g. hGetLine vs hGetContents

-- We rewrite the imperative chunking code from before using lazy I/O style:
chunkStream :: Handle -> IO [L8.ByteString]
chunkStream h
 = do
   isEof <- hIsEOF h
   if isEof
     then return []
   else do
     chunk <- LB.hGet h 8
     rest <- (chunkStream h)
     return (chunk:rest)

-- We can now produce a stream and consume it
mainLazy6 = do
  chunks <- chunkStream h
  print $ take 10 chunks

-- Consumer:

