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
processChunk :: String -> [L8.ByteString] -> IO ()
processChunk acc [] = do putStrLn acc -- terminate recursion
processChunk = processChunk' ""

processChunk' acc (chunk:chunks)
 = case (parseChunk chunk) of
   (Chunk chunk') -> do
     processChunk' (acc ++ chunk') chunks
   (LineEnd chunk' remainder) -> do
     let line = acc + chunk'
     putStrLn line -- process line
     processChunk' remainder chunks

-- ProcessChunk recursively consumes our stream and accumulates chunks into lines. It is tail recursive and uses constant space.

mainLazy7 = do
  h <- openFile "test.txt" ReadMode
  chunkStream h >>= processChunk -- producer and consumer are decoupled
  hClose h

-- In the imperative example, loop function drives the iteration through the chunks and consumer was not explicit.

-- processChunk method is an I/O action with side effects - as it loops through the file chunks, it accumulates chunks until it has captured a whole line. It then does some I/O with the line and starts accumulating chunks for the next line.

-- Iteration and I/O processing are therefore interleaved.
-- We can decouple this further by making a pure lineStream function that produces a stream of lines:

toLines = lineStream ""

lineStream accChunks [] = [accChunks]
lineStream accCHunks (chunk:chunks)
 = case (parseChunk chunk) of
   (Chunk chunk')             -> lineStream (accChunks ++ chunk') chunks
   (LineEnd chunk' remainder) -> (accChunks ++ chunk') : (lineStream remainder chunks)

-- ChunkSteam can now be passed into the toLines function:
mainLazy8 = do
  h <- openFile "test.txt" ReadMode
  lines' <- liftM toLines (chunkStream h)
  mapM_ putStrLn lines'
  hClose h

-- toLines -> pure function
-- chunkStream -> stream wrapped in I/O monad
-- liftM is therefore used to lift the toLines function into the Monad.
-- Alternative:
--   chunks <- (chunkStream h)
--   let lines' = toLines chunks

-- It is only when mapM_ putStrLn lines' is called that the stream toLines materializes, which drives the evaluation of chunkStream.
-- Underneath the mapM_ function, the sequence_ function drives the evaluation of the stream.

mainLazy9 = 
  h <- openFile "test.txt" ReadMode
  lines' <- hGetContents h >>= return . Lines
  mapM putStrLn lines'
  hClose h

-- Here we composed pure functional streams with I/O streams using monadic operators and functions.

-- Pros of Lazy I/O:
-- I/O expressed at relatively high level of abstraction
-- Very composable, enabling decoupling of producers from consumers

-- Cons of Lazy I/O:
-- Poor control over when something is evaluated
-- Poor control of resources

-- The Problems with Lazy I/O

mainLazy10 = do
  h <- openFile "test.txt" ReadMode
  firstLine <- hGetLine h     -- returns string immediately
  contents  <- hGetContents h -- returns a promise
  hClose h
  print $ words firstLine -- prints the string
  print $ words contents  -- prints nothing (live stream closed when hClose is called)

-- Some problems with Lazy I/O:
-- 1. Order of side effects is tied to order of lazy evaluation. Because order of lazy evaluation if not explicit, the order of effects also isn't.
-- 2. Can be difficult to reason about space requirements of a lazy program.
-- 3. Poor resource management and lack of explicit order of effects can make it difficult to know when to clean up resources.

-- Lazy I/O is good for simple situations, where space requirements and order of execution are fairly predictable.
-- However when we need precise resource management or predictable space usage, lazy I/O is not an option.

-- Resource management with bracket

-- Examples so far have used explicit resource management:
mainLazy11 = do
  h <- openFile "test.txt" ReadMode
  useResource h
  hClose h
  where
    useResource h' = (stream h') >>= mapM_ putStrLn
    stream h'      = hGetContents h' >>= return . lines

-- We can use higher level abstractions to capture this common pattern.
-- Simplest is just to ignore the problem and let the garbage collector clean up (not a good idea):
mainLazy12 = do
  contents <- readFile "test.txt"
  mapM_ putStrLn (lines contents)

-- A more idiomatic approach is to use the withFile wrapper function:
mainLazy13 = do
  withFile "test.txt" ReadMode enumerateLines
  where
    enumerateLines h = lines' h >>= mapM_ putStrLn
    lines' h' = hGetContents h' >>= return . lines

-- withFile decouples producer from consumer and gives better control over resource management.
-- The file will be closed in case of completion or error because withFile makes use of the bracket function:

bracket
  (openFile "filename" ReadMode) -- acquire resource
  hClose                         -- release resource
  (\h -> "do some work")         -- action function
where
  bracket :: IO a         -- before action
          -> (a -> IO b)  -- after action
          -> (a -> IO c)  -- do action
          -> IO c         -- result

-- The finally function is a special form of bracket:
finally :: IO a -- main action
        -> IO b -- final action - runs afterwards
        -> IO b -- result

-- bracket functions help us to clean up resources more reliably but do not definitively solve
-- the problem of closing resources in a timely manner

-- If we require more precise resource management, control of space requirements, and control of ordering of effects,
-- then we must use Iteratee I/O


-- Iteratee I/O

-- Iteratee I/O combines the precise resource management and space requirements of Handle-based I/O with the decoupling of
-- producers and consumers possible with lazy I/O.

-- Using previous example:
data Chunk' = Chunk' {chunk :: String} | LineEnd' {chunk :: String, remainder :: String}

parseChunk' :: ByteString -> Chunk'
parseChunk' chunk
 = if rightS == B8.pack ""
     then Chunk'   (toS leftS)
     else LineEnd' (toS leftS) ((toS . B8.tail) rightS)
   where
     (leftS, rightS) = B8.break (== '\n') chunk

toS = map (chr . fromEnum) . B.unpack

-- We model the iteration step as a function that processes a file chunk and returns an IterResult value:
-- iterF :: String -> IterResult

-- N.B. newtype == data keyword except with newtype there must be exactly one constructor with exactly one field inside it.
-- This type represents the concept of an 'iteratee':
newtype Iter = Iter {runIter :: B8.ByteString -> IterResult}

-- The IterResult can indicate 2 possibilities, HaveChunk and NeedChunk:
data IterResult
 = HaveLine {line :: String, residual :: String} -- a line has been accumulated (with possible residual)
 | NeedChunk Iter -- need more chunk data

instance Show IterResult where
  show (HaveLine l r) = "HaveLine " ++ l ++ "|" ++ r
  show (NeedChunk _)  = "NeedChunk"

-- The HaveLine function simply returns some date fields, but the NeedChunk function is not so simple:
-- NeedChunk (String -> IterResult)
-- Instead of returning a value, NeedChunk returns an Iter function i.e. another step function.
-- This is the basis of Iteratee I/O: a step function can return another step function

-- Iteratee

-- Next we write an Iter function:

chunkIter :: Iter
chunkIter = Iter (go "")
  where
    go :: String -> B8.ByteString -> IterResult
    go acc chunk =
      case (parseChunk chunk) of
        (Chunk chunk')             -> NeedChunk (Iter (go (acc + chunk')))
        (LineEnd chunk' residual') -> HaveLine (add ++ chunk') residual'

-- Note we curry go with the acc parameter so that we get the correct Iter type signature: B8.ByteString -> IterResult

-- Below the first run of chunkIter returns NeedChunk. This lets the caller know that another iteration should be performed.
-- The iteratee provides us with the next iteratee to run (which internally knows about the chunks accumulated so far):
mainLazy14 = do
  h <- openFile "test.txt" ReadMode
  chunk1 <- B.hGet h 25
  let (NeedChunk iter1) = runIter chunkIter chunk1 

  chunk2 <- B.hGet h 25
  let (HaveLine line residual) = runIter iter1 chunk2
  putStrLn line

-- Enumerator

enumerateFile path initIter =
  withFile path ReadMode $ \n ->
    let
      go iter = do
        isEOF <- hIsEOF h
        if isEOF
          then return (HaveLine "End Of File" "")
          else do
            chunk <- B.hGet h 8
            check $ runIter iter chunk

      check (NeedChunk iterNext) = go iterNext
      check (HaveLine line residual)
        = do
            putStrLn line
            check $ runIter initIter (B8.pack residual)
            in go initIter

mainLaxy15 = do enumerateFile "test.txt" chunkIter

-- Enumerator has type:
enumerateFile :: FilePath -> Iter -> IO IterResult

-- If we apply the first argument to enumerateFile we get: (enumerateFile file) :: Iter -> IO IterResult
type Enumerator = Iter -> IO IterResult

-- Therefore -> enumerateFile :: FilePath -> Enumerator

-- Enumerators work like folds. They apply a function (with an accumulator) element by element over an input stream.


-- Generalized iteratees, enumerators, and enumeratees

-- Our current code is still along way from being a robust and composable Iteratee I/O.

-- Currently the last line of the file is lost. To solve we replace ByteString with:

data IterInput = Chunk' String | EndOfFile

-- At the EOF the enumerator can pass in the EndOfFile to the iteratee and ask it to return the last line as a HaveLine function.

-- To deal with failure:

data IterResult
  = HaveLine {line :: String, residual :: String}
  | NeedChunk Iter
  | Failure {errMsg :: String}

-- The iteratee and enumerator types should also be generalised by parameterising our types.

-- In order to compose iteratees and enumerators, we need to make them implement the monad type class.

-- We also need the enumeratee's abstractions that enable transforming the output of an enumerator or iteratee and feeding that into another iteratee.

-- Iteratees produce data.
-- Enumeratees serve as pipeline transformers of data.
-- Enumerators consume data and drive the pipeline process.

-- The iteratee can also influence evaluation by signalling:
--    When it has finished processing
--    When it needs more data
--    When it can yield a result
--    When it encounters a failure

-- This flexibility means that resource management in the face of exceptions becomes much more traceable than with Lazy I/O.
-- Resource management can also be abstracted at a much higher level than Handle-based I/O.

-- The Iteratee I/O libraries

-- Iteratee libraries vary widely in how they model types for iteratees, enumerators, and enumeratees.
-- The meaning and necessity of enumeratees varies between libraries.
-- The types of signals the iteratee can send back to the consumer also differ.

-- Some libraries:
-- IterateeM/CPS - Oleg Kiselyov - early groundword
-- Iteratee - John Lato + Oleg Kiselyov - clarified key concepts
-- Enumerator + iterIO - John Millikin - simplified mental model
-- Pipes - Gabriel Gonzales - focus on preserving equational reasoning
-- Conduit - Michael Snoyman - focus on deterministic resource management

-- Comparing 3 styles of I/O

-- Handle based I/O:
--   Strict & incremental processing
--   Evaluation driven by looping code
--   Low abstraction level
--   Precise resource management

-- Lazy I/O:
--   Lazy processing
--   Evaluation driven by stream consumer
--   High abstraction level
--   No precise resource management

-- Iteratee I/O:
--   Strict & incremental processing
--   Evaluation driven by enumerator & iterator
--   High abstraction level
--   Precise resource management
