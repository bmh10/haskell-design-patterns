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

-- The law of composition of Applicative Functor (where . is function composition operator):

pure (.) <*> u <*> v <*> w = u <*> (v <*> w)

-- When we combine 2 Applicatives with the <*> operator, we always get another Applicative.
-- It is said the Applicatives are therefore "closed under composition".

-- Since Applicative is a Functor we can use the fmap function:

pure (,) <*> Just' 2 <*> Just' 3
-- same as:
(fmap (,) (Just 2))  <*> Just' 3
((,) <$> (Just 2))   <*> Just' 3
-- same as:
(,) <$> (Just 2)     <*> Just' 3

-- where <$> is infix synonym for fmap.

-- Applicative provides us with a multiparameter fmap function.
-- <*> operator is more generic (than <$>) and uniformly lifts function application to the Functor level:

fmap, (<$>) ::   (a -> b) -> f a -> f b
(<*>)       :: f (a -> b) -> f a -> f b

-- Monad

-- Monad inherits from Applicative:

class (Applicative m) => Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b

-- return is like the pure function from Applicative. It wraps a value in a Monad class.

-- The bind (>>=) operator combines a Monad (m a) with a Monadic function (a -> m b).
-- The Monadic function acts on type a of the first monad and returns a new monad of type (m b).

-- Lets make Maybe' a Monad class:
data Maybe' a = Just' a | Nothing' deriving (Show)

instance Functor Maybe' where ...
instance Applicative Maybe' where ...

instance Monad Maybe' where
  return x = Just' x
  Nothing'  >>= _ = Nothing' -- Given Nothing', ignore the monadic function, and return Nothing'
  (Just' x) >>= f = (f x)    -- Given Just' x, apply the monadic function to the value x. This will return a Maybe' monad.

-- Example:

-- monad >>= monadic function
Just' 10 >>= \x -> Just' (show x)
-- evals to:
(\x -> Just' (show x)) 10
Just' "10"

-- At first it seems that bind simply allows us to combine monads with monadic functions.
-- It is not quite that simple.

-- Monad as Functor

-- We can change Monad to the Functor class using the 'liftM' function, monadic version of fmap:

instance Functor Monad where
  fmap = liftM
  where
    liftM :: Monad   m => (a -> b) -> m a -> m b
    fmap  :: Functor m => (a -> b) -> m a -> m b

-- Levels of Functor-like behaviour:

main = do
  print $ fmap (*2) (Just' 10)     -- Functor
  print $ pure (*2) <*> (Just' 10) -- Applicative
  print $ liftM (*2) (Just' 10)    -- Monad

-- Monad liftM function is based on bind operator:

liftM f m = m >>= return . f

-- or:
liftM f m = do
  val <- m        -- extract value of Monad m
  return (f val)  -- pass value to Monadic function f and wrap/lift result into Monad

-- e.g.
liftM (*2) (Just' 10)
= (Just' 10) >>= return . (*2) -- using liftM definition
= return . (*2) 10             -- extract value of (Just' 10) Monad and apply Monadic function
= return 20      
= Just' 20                     -- wrap/lift result into Monad

-- Monad as applicative

-- Applicatives can lift functions with many arguments.
-- Monads can also do this less elegantly with liftM functions e.g. liftM2, liftM3, ...

main = do
  print $ (<$>)  (*) (Just' 10) <*> (Just' 20) -- Applicative
  print $ liftM2 (*) (Just' 10)     (JUst' 20) -- Monad

-- Any monad is also an applicative functor:

-- ap_ defines <*> for Monads
-- 'ap' stands for 'applicative pattern'
ap_ mf mx = do
  f <- mf      -- extract function
  x <- mx      -- extract val
  return (f x)

-- We extract the function from the first Monad and the value from the second.
-- We then do function application (this function is already in Control.Monad.ap).

-- Now we can write monadic code in applicative style:
(Just' (*)) 'ap_' (Just' 10) 'ap_' (Just' 20)

-- We can easily make Monad an instance of Applicative:

instance Applicative Monad where
  pure = return
  (<*>) = ap_

-- The applicative pattern was only reconginzed in 2008.
-- This is why there a several ways of donig the same thing:

-- Functor   Applicative   Monad
-- fmap      pure, <*>     liftM
--           <*>           ap
--           pure          return

-- These discrepancies were resolved by the "Functor-Applicative Monad Proposal"
-- See https://wiki.haskell.org/Functor-Applicative-Monad_Proposal


-- Sequencing actions with Monad and Applicative

-- Monad can sequence actions:

action s = do putStrLn s; return s
main = do
  let actions = map action ["parts", "are", "disconnected"]
  sequence' actions
  return ()

-- sequence' performs actions one after the other:
sequence' [] = return []
sequence' (x:xs) = do
  x'  <- x            -- action performed
  xs' <- sequence' xs
  return (x':xs')

-- We can also sequence actions with Applicative:
sequenceA [] = pure []
sequenceA (x:xs) = (:) <$> x <*> (sequence xs)

-- Part of the Functor-Applicative-Monad proposal is to change the prelude's sequence function
-- to require applicative instead of monad.

-- Applicative can sequence actions that happen in isolation i.e. each stage does not rely on results of previous stage.
-- However, when actions need to communicate results to subsequent actions, Monad is required.


-- Monads and the Bind Chain

-- With Monads we can build a chain of actions such that an action can communicate with subsequent actions.

-- For example, the following can be expressed using Monads but not with Applicative:

main = do
  line <- getLine                -- action 1
  putStrLn $ "You said " ++ line -- action 2 (uses result of action 1)

-- Bind (>>=) lets us bind outputs to inputs, whereas with Applicative (<*>) each action is performed in isolation.

-- This also means that with Monads we can have a dynamic sequence of actions, where an action's outcome can affect
-- which action is executed next e.g.

main = mainLoop
mainLoop = do
  line <- getLine -- action 1
  if line == "stop"
    then putStrLn "Bye" -- action 2b
    else do
      putStrLn $ "You said " ++ line -- action 2c
      mainLoop


-- Note that Applicative does allow for limited communication between actions e.g.
(+) <$> Nothing <*> Just 10 <*> Just 20

-- Here Nothing will prevent all subsequent actions from being performed.
-- Communication between actions is baked into the Maybe Applicative type instance.


-- While working with the Reader Monad earlier, we saw that we can use the bind chain to provide a "shared context" between actions in a sequence.
-- Can use shared context to do "out of band" processing -> processing that is explicit in the bind chain but implicit from the perspective
-- of the chain of monadic actions.
-- e.g. in the Reader Monad the reader state is out of band i.e. independent of the monadic pipeline.
-- This is the reason we can use monads to approximate imperative programming.

-- We can also view the bind chain as a more sophisticated version of an accumulator argument in a tail recursive function.
-- Accumulator arguments allow for a shared context in a nested chain of recursive function calls.
-- Monads bind in a way that includes an "accumulator".

-- Bind (>>=) operator composes Monad values with monadic functions (functions that return a value embedded in a Monad class).

-- Monads are not "closed under composition" like Applicatives. Monads generally don't compose into Monads.


-- Composing with monads

-- We can compose pure functions with Monads:
liftM* f m -- returns another Monad m

-- We can compose monadic functions with Monads:
m >>= fM >>= gM >>= hM

-- We can compose monadic functions with each other:
gm <=< fM

-- <=< is syntactic sugar for:
gm <=< fM = \x -> (fM x) >>= gM

-- The key composition is binding (>>=) the monad with a monadic function.
-- Beyond that monads don't compose as well as applicatives.


-- Monad Transformers

-- We look at combining different types of monads into more powerful combinations.
-- This can be done by creating "Monad stacks".

-- We start with a Reader Monad to hold some configuration data for an application:

data Config = Config {discountRate :: Float, currentSym :: String}

appCfg = (Config 10 "R")

-- dicount function takes a Float and returns a Float in the context of a Reader Config:

discount :: Float -> Reader Config Float
discount amt = do
  discountRate' <- asks discountRate
  return (amt * (1 - discountRate' / 100))

-- From within the function, we ask for the config data.

main = do
  print $ runReader (discount 100) appCfg

-- Lets add a display function in the context of Reader Config:

display :: Float -> Reader Config String
display amt = do
  currencySym' <- asks currencySym
  return (currencySym' ++ " " ++ (show amt))

main = d0
  putStrLn $ runReader doDoubleDiscount appCfg
where doDoubleDiscount
 = (discount 100 >>= discount >>= display)

-- Here we have a chain of monadic functinos executing in the context of Reader Config.
-- To add logging capability to our Reader functions, we can stack our Reader type on top of a Writer Monad.

-- ReaderT type is a Reader Monad that also takes an inner Monad, in this case Writer String.

discountWR :: Float -> ReaderT Config (Writer String) Float
discountWR amt = do
  discountRate' <- asks discountRate
  let discounted = amt * (1 - discountRate' / 100)
  tell $ "-Discount " ++ (show amt) ++ " = " ++ (show discounted)
  return discounted

displayWR :: Float -> ReaderT Config (Writer String) String
displayWR amt = do
  currencySym' <- asks currencySym
  tell " > Displaying..."
  return (currencySym' ++ " " ++ (show amt))

main = do
  print $ runWriter (runReaderT doDoubleDiscount appCfg)
  where doDoubleDiscount = (discountWR 100 >>= discountWR >>= displayWR)

-- We first user runReaderT, which unwraps the Reader Monad and gives us the result.
-- The Reader function's result in the inner Writer Monad, which is unwrapped using runWriter.

-- Note that Monad stack are unwrapped in reverse order of their wrapping.

-- In order to simplify things we can introduce:

type App = ReaderT Config (Writer String)

-- The types then become:

discountWR :: Float -> App Float
displayWR  :: Float -> App String

-- We can now define a doApp function:

doApp :: App a -> (a, String)
doApp app = runWriter (runReaderT app appCfg)

main = do
  print $ doApp doDoubleDiscount
  where doDoubleDiscount = (discountWR 100 >>= discountWR >>= displayWR)

-- More idiomatically we can use the newtype method:

newtype App a = App {runApp :: ReaderT Config (Writer String) a}
  deriving (Monad, MonadReader Config, MonadWriter String)

-- The 'deriving' clause requires a language pragma at the top of the file:

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- Reader & ReaderT both implement MonadReader
-- Writer & WriterT both implement MonadWriter

-- The effect of this is to flatten out nested Monad stack, so that all lower-level functions are
-- available at the top-level. e.g. we can call tell and asks as if they were defined on the App monad level.

-- Without this we would have to lift the nested Monad funtion for each level of the stack until reaching the monad
-- with the function we want to use. The flattening of nested monads lets us avoid nested lifting.

-- Downside is that this practical simplification is precisely what makes defining your own Monad transformers tedious.
-- Some packages have tried to solve this, see: 'extensible-effects' and 'layers'.

-- Using newtype also has the advantage that we can limit what we export from our code and therefore hide any details
-- we don't want to expose.

-- However, in doApp we now need to use the runApp function:

doApp :: App a -> (a, String)
doApp app = runWriter (runReaderT (runApp app) appCfg)

main = do
  print $ doApp doDoubleDiscount
  where doDoubleDiscount = (discountWR 100 >>= discountWR >>= displayWR)

-- IO in monad stacks

-- We can add the IO Monad to our stack. The IO Monad is a special case:

newtype AppIO a = AppIO {runAppIO :: ReaderT Config (WriterT String IO) a
                  deriving (Monad, MonadReader Config, MonadWriter String, MonadIO)}

-- Instead of Writer String we use WriterT String IO. i.e. Writer Monad wrapping IO.

discountWRIO :: Float -> AppIO Float
displayWRIO  :: Float -> AppIO String

doAppIO :: AppIO a -> IO (a, String)
doAppIO app = runWriterT (runReaderT (runAppIO app) appCfg)

-- doAppIO returns previous result wrapped in IO action.
-- runWriterT is used to extract the writer result

-- Now we can do IO operations in our Monad stack functions, using liftIO to expose the IO Monad:

discountWRIO amt = do
  liftIO $ putStrLn "We're doing IO!"
  discountRate' <- asks discountRate
  let discounted = amt * (1 - discountRate' / 100)
  tell $ " > Discounting " ++ (show amt) ++ " = " ++ (show discounted)
  return discounted

displayWRIO amt = do
  liftIO $ "More IO!"
  currencySym' <- asks currencySym
  tell " > Displaying..."
  return (currencySym' ++ " " ++ (show amt))

main = do
  print <$> doAppIO doDoubleDiscount
  where doDoubleDiscount = (discountWRIO 100 >>= discountWRIO >>= displayWRIO)

-- Sequence of stack composition

-- A Monad stack implies a sequence of composition of Monads.
-- Unwrapping always done is opposite order to wrapping.
-- With Reader and Writer Monads the order is inconsequential since they do not affect each other in the stack.
-- However, if one monad relies on the work of a previous monad being done, the order matters.

-- The IO monad is a sepcial case and must remain at the bottom of the stack.


-- Arrows

-- Lets look at Arrows from the perspective of Monads. Consider:

import System.IO
main = do
  liftM (length . words)
        (readFile "jabberwocky.txt") >>= print

-- regular functions: length, words
-- monadic functions: readFile, print

-- liftM lifts the composed function length . words into the monadic function readFile, which is then fed into the result of another monadic function print. 

-- Regular functions can be composed with (.) but we cannot do the following:
print . length . words . readFile "jabberwocky.txt" -- invalid

-- However we can make this possible.

-- Our approach requires the creation of a "meta type" to represent monadic IO functions.
-- We then define composition for that type.

data IOF a b = IOF {runIOF :: a -> IO b}

-- IOF wraps a function (a -> IO b) and places the input and output types (a and b) on equal footing, while also hiding the IO monad.
-- We define our own composition operator:
(<<<<) :: IOF a b -> IOF c a -> IOF c b
(IOF f) <<<< (IOF g) = IOF $ f <=< g

-- The function takes 2 IO functions, composes them, and wraps the resulting IO function.

-- Finally we need a function to lift a Monadic IO function into an IOF variable:

lift' :: (a -> b) -> IOF a b
lift' f = IOF $ return . f -- uses IO Monad's return

-- Now we can compose regular and IO functions:
main = do
  let f = IOF print <<<< lift' length <<<< lift' words <<<< IOF readFile
  runIOF f "test.txt"
  return ()

-- By doing this we have started to reinvent arrows. Now lets do the same but using the arrow type-class.

-- Implementing an arrow

-- To write an arrow we need the IOF type from the previous example, however this time we call it IOArrow:

data IOArrow a b = IOArrow {runIOArrow :: a -> IO b}

-- To make IOArrow a true arrow we need to implement Category and Arrow.
-- Category describes function composition:

instance Category IOArrow where
  id = IOArrow return
  -- (.) = (<<<<)
  IOArrow f . IOArrow g = IOArrow $ f <=< g

instance Arrow IOArrow where
  -- arr = lift'
  arr f = IOArrow $ return . f
  first (IOArrow f) = IOArrow $ \(a, c) -> do
    x <- f a
    return (x, c)

-- The arr function is just lift' from earlier but using IOArrow instead of IOF.
-- Recall, 'return' lifts a value into a Monad and 'pure' lifts a value into an Applicative.
-- Similarly, arr lifts a value into an Arrow

-- Example of Arrow use:

import Prelude hiding ((.), id)
import Control.Category
import Control.Applicative
import Control.Arrow
import Control.Monad
import System.IO

main = do
  let f = IOArrow print . arr length . arr words . IOArrow readFile
  runIOArrow f "test.txt"


-- Arrow Operators

-- By defining arr, first, and (.) on the Category instance, we get other operators for free.
-- The (<<<) operator defines Arrow composition. It is the same as the (.) function composition defined in the arrow's Category instance:

(<<<) :: Category cat => cat b c -> cat a b -> cat a c

-- In above example we could have used (<<<) instead of (.)
-- Another operator (>>>), is composition reversed:
(>>>) = flip (.)

-- Example of 'first':

main = do
  let f = (IOArrow readFile) >>>
          (arr words) >>>
          (arr (\x -> (x, x))) >>> -- split stream in 2
          (first (arr length)) >>> -- use first tuple value 
          (IOArrow print)
  runIOArrow f "test.txt"

-- Now out final result is a tuple, the first part containing the word count and the second part containing the original words.
-- Using this method we can create "side channels" in our pipelines, which allows us to share state across arrows in a pipeline.

-- The 'second' operator works on the second part of the arrow's input:

main = do
  let f = (IOArrow readFile) >>> 
          (arr words) >>>
          (arr (\x -> (x, x))) >>>
          (first (arr length)) >>>
          (second (arr head)) >>>
          (IOArrow print)
  runIOArrow f "test.txt"

-- Now we are doing 2 different computations on the different branches.
-- Finally we look at the (***) operator:

main = do
  let f = (IOArrow readFile) >>>
          (arr words) >>>
          (arr (\x -> (x, x))) >>>
          (arr length *** arr head) >>>
          (IOArrow print)
  runIOArrow f "test.txt"

-- This does the same as using first and second - it runs 2 Arrows on the first and second tuple values.

-- This ability allows Arrows to take multiple inputs and therefore we can build pipelines with branching and merging of stream values.


-- Kleisli arrows and monad arrows

-- The IOArrow type we defined is actually unnecessary because there is already a Kleisli arrow, which generalizes IOArrow to all Monads.
-- It is defined in Control.Arrow:
data Kleisli m a b = K {runKleisli :: a -> m b}
instance Monad m => Arrow (Kleisli m) where
  arr f = K (\x -> return (f x))
  K f >>> K g = K (\x -> f x >>= g)

-- Using Kleisli arrows we could have just written:

main = do
  let f = Kleisli print . arr length . arr words . Kleisli readFile
  runKleisli f "test.txt"

-- Arrows generalize Monad.
-- For every Monad type, there is a corresponding Arrow type. But there are more Arrows than Monads.

-- From most to least general (and least to most powerful): Functor -> Applicative -> Arrow -> Monad

-- Much like Monad stacks, we can combine different arrows into stacks using transformer Arrows.


-- Why Arrows?


