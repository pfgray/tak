import Data.Char (toUpper, toLower) -- used for an example or two
import Data.Semigroup hiding (Option)

{-| Today we're working with Applicative Functors (or 'Applicative's for not-quite-as-long).  As
I'm sure you recall, the applicative class looks like this:

@
    class Functor f => Applicative f where
      pure  :: a -> f a
      (<*>) :: f (a -> b) -> f a -> f b
@

The type signature for '(<*>)' should look very similar to that of 'fmap' (which we can also write
as '(<$>)').  This reinforces the perspective that we're viewing @f@-world as a place where
@f@-effects can happen, but is otherwise just Haskell.  With 'fmap', we gained the power to apply
pure functions to effectful values.  Now with '(<*>)', we gain the ability to apply functions that
already exist in @f@-world to values in @f@-world.  This feels like an extremely modest increase in
power, but as we'll see, it's actually a huge leap in expressive power.

For today's homework, I'm going to show you how the 'Maybe' applicative works (using our old friend
'Option'), and then you're going to work through several (hopefully somewhat novel) examples
yourselves.

-}

-- | First me: Here's 'Option', and its 'Functor' and 'Applicative' instances:

data Option a = None | Some a
  deriving (Eq, Ord, Show)

instance Functor Option where
  fmap _ None     = None
  fmap f (Some a) = Some (f a)

instance Applicative Option where
  pure = Some
  Some f <*> Some x = Some (f x)
  _      <*> _      = None

-- | I'm going to immediately stop using 'Option' now, because I keep typing 'Just' when I want to
-- type 'Some' and it's annoying, and the example has served its purpose.  The 'Maybe' applicative
-- is exactly the same thing.  Observe that in a 'Maybe'-world, any failing computation (that is, a
-- 'Nothing') causes the entire computation to fail.  Here's a cutesy example of using it:

data Salutation = Hello | Goodbye | What'sUp | Tata
  deriving (Eq, Ord, Show)

salutations :: [(Salutation, String -> String)]
salutations = [(Hello, hello), (Goodbye, goodbye)]
  where hello n = "Welcome, " <> n <> "!"
        goodbye n = n <> ", farewell!"

-- | Since there are more 'Salutation's than there are elements of 'salutations', we might not
-- actually find our desired greeting in the list.
--
-- >>> greet Hello "Bob"
-- Just "Welcome, Bob!"
--
-- >>> greet Tata "Cynthia"
-- Nothing
greet :: Salutation -> String -> Maybe String
greet s n = lookup s salutations <*> pure n

-- | Okay, now back to you!  There's a useful function named 'traverse' that we're going to talk
-- about next week or the week after.  We're going to write a slightly simpler version of it.  Take
-- a look at the type.  Let's translate that into English: "Given a function which can compute a @b@
-- from an @a@ (possibly with some @f@-effects) and a list of @a@, give me the @f@-world list of @b@
-- you get by applying it to each element of the list."  Read that two or three times, and I think
-- it'll sink in.

-- >>> foreach (`lookup` [(1,"foo"), (2, "bar")]) [1,2]
-- Just ["foo","bar"]
--
-- >>> foreach (`lookup` [(1,"foo"), (2, "bar")]) [1,2,3]
-- Nothing
--
-- NB: Don't forget that you once knew how to write simple recursive functions on lists!
foreach :: Applicative f => (a -> f b) -> [a] -> f [b]
foreach _ [] = pure []
foreach f (x:xs) = (fmap (flip (:)) (foreach f xs)) <*> f x

isLong s = if ((length s) > 3) then Nothing else Just s 

-- | Actually, there's a way to break 'foreach' down into combinators.  However, one of those
-- combinators is called 'sequence' (which we'll call 'commute' here).  It "commutes" (in the
-- metaphorical sense of the commutative law) the @f@ and the '[]' functors.  With 'commute', we
-- could implement 'foreach' as @foreach f = commute . fmap f@.  If we wanted to.  I guess.
commute :: Applicative f => [f a] -> f [a]
commute [] = pure []
commute (x:xs) = (fmap (flip (:)) (commute xs)) <*> x 

-- | Ok, now let's try some new stuff.  Let's make an applicative for counting stuff.  A simplifying
-- assumption here that we'll generalize later is that for now, the thing we're going to count is
-- the number of times someone used 'pure' in our computation.  A value of type @Count a@ is just
-- like an @a@, but it carries a running total of whatever we're counting with it.
newtype Count a = Count { runCount :: (Int, a) }
  deriving (Show)

-- | Instantiate 'Functor' for 'Count'.
instance Functor Count where
  fmap f c = Count (fmap f (runCount c))

-- | Instantiate 'Applicative' for 'Count'.  Remember that we're counting the number of times that
-- 'pure' has been used in the computation.
--
-- >>> runCount $ (+) <$> pure 5 <*> pure 8
-- (2,13)
--
-- >>> runCount $ foreach pure "foobar"
-- (7, "foobar")
instance Applicative Count where
  pure x = Count (1, x)
  Count (n, ab) <*> Count (n', a) = Count (n + n', ab a) 

-- | Remember @From@ from last week?  I'm going to rename it 'Conf', because the effect it embodies
-- is like having a bit of configuration around that you can always refer to if you want.  So a
-- @Conf (Username, Password) Bool@ might represent the computation of a 'Bool' in a world where you
-- can reach out for a username-password pair when you want it.
newtype Conf r a = Conf { runConf :: r -> a }

-- | Here's an example of using the 'Conf' applicative (you'll instantiate it below).
data Volume = Quiet | Loud
  deriving (Eq, Ord, Show)

say :: Volume -> String -> String
say Quiet = fmap toLower
say Loud  = fmap toUpper

-- | Notice that this uses 'ask', which you'll implement in a moment.
--
-- >>> runConf (orate ["Greetings", "Fellow", "Citizens"]) Loud
-- "GREETINGS\nFELLOW\nCITIZENS\n"
--
-- >>> runConf (orate ["GREETINGS", "FELLOW", "CITIZENS"]) Quiet
-- "greetings fellow citizens"
orate :: [String] -> Conf Volume String
orate speech = say <$> ask <*> pure (unwords speech)

-- | Think about what 'ask' must do in 'orate' above.  Somehow, it gets the 'Volume' from the
-- configuration and makes it available to 'say'.  Remember that the "configuration" is just the @r@
-- in the @r -> a@.
ask :: Conf r r
ask = Conf id

-- | Instantiate 'Functor' for 'Conf'.  Remember you've already done this last week with 'From'.
instance Functor (Conf r) where
  fmap f (Conf ra) = Conf (f . ra)

-- This instance is tricky.  I promise it's just type-tetris.
instance Applicative (Conf r) where
  pure a = Conf $ const a 
  Conf rab <*> Conf ra = Conf (\r -> (rab r) (ra r))

-- | A slight generalization of 'ask' is 'asks'.  It lets you conveniently call a function on your
-- configuration when you request it.  It has the following type (implying that @ask = asks id@):
asks :: (r -> a) -> Conf r a
asks f = fmap f ask

-- | Use 'asks', and the following functions (all already in scope) to (ineffeciently) give the
-- number of characters, words, and lines in a String in your configuration.
--
-- @
--     words :: String -> [String]
--     lines :: String -> [String]
-- @
--
-- >>> runConf wc "Whan that Aprille\nwith his shoores soote\nthe drought of march\nhath perced to the roote"
-- (86,16,4)
--
-- NB: Observe that we have a computation that really actually does some actual work, even though
-- it's not really a function the way you'd normally write it.  Getting used to the idea that we
-- have stable values that are in some sense "active" will be worthwhile, especially when we start
-- talking about 'IO', where you can have computations like @getLine :: IO String@, which reads a
-- String from the console, but doesn't actually take any arguments.
wc :: Conf String (Int, Int, Int)
wc = (pure (\a b c -> (a, b, c))) <*> (fmap length ask) <*> (fmap length $ asks words) <*> (fmap length $ asks lines)

-- | Okay, now let's make 'Count' a tad less awkward.  We'll call it the 'Log' applicative.
newtype Log l a = Log { runLog :: (l, a) }
  deriving (Show)

-- | The 'Functor' instance should be straightforward---almost the same as for 'Count'.
instance Functor (Log l) where
  fmap f lg = Log $ fmap f $ runLog lg

-- | However, the 'Applicative' instance requires some ingenuity.  As you've noticed, we're
-- generalizing by being polymorphic in the type of the log we're going to be accumulating.  When we
-- knew that our "log" type was 'Int', we were able to make some concrete decisions in our
-- implementation of our instance.  So I'm going to get you started with the declaration for our
-- more general instance:
instance Monoid l => Applicative (Log l) where
  pure x = Log (mempty, x)
  Log (l, ab) <*> Log (l', a) = Log (l <> l', ab a)

-- | Much as we need 'ask' to help us make decent use of the 'Conf' applicative, we're going to need
-- some way of logging values we care about.  But logging doesn't really /produce/ a value, so I
-- believe this will be our first nontrivial use of '()'!
tell :: l -> Log l ()
tell l = Log (l, ())

-- | Here's how to use our 'Log' applicative to implement 'filter' in a funny way.  Hopefully, this
-- will give an indication of how the 'Monoid' we need functions with 'tell'.
--
-- >>> weirdFilter even [1..10]
-- [2,4,6,8,10]
weirdFilter :: (a -> Bool) -> [a] -> [a]
weirdFilter p xs = fst . runLog $ foreach go xs
  where go x | p x       = tell [x]
             | otherwise = pure ()

-- | Here's a weird factorial:
weirdFactorial :: Int -> Int
weirdFactorial n = getProduct . fst . runLog $ foreach (tell . Product) [1..n]

-- | Notice that we threw away the "value" that the 'foreach' computed in 'weirdFilter' and
-- 'weirdFactorial'.  That's because they're just a list of '()', which isn't terribly useful.  Now
-- that we're playing in the world of effects, sometimes we only really care about the effect we're
-- computing, rather than the "value" of the function.  There are a couple functions that can help
-- us ignore the results of computations we only want to run "for effect".
--
-- @
--     (<*) :: Applicative f => f a -> f b -> f a
--     (*>) :: Applicative f => f a -> f b -> f b
-- @
--
-- It's important to know that '(<*)' is /not/ 'const', even though 'const' has a compatible type.
-- This is because it's important that the /effect/ of @f a@ and @f b@ both happen, and in the
-- correct order (for the same reason, '(*>)' isn't @flip const@ either).
--
-- So let's implement 'foreach_', (it's called 'traverse_' when imported from 'Data.Foldable') which
-- will work like 'foreach', but will simply discard the results of each function application.  Use
-- '(*>)'.
--
-- >>> runLog $ foreach_ (\x -> tell [x]) "hello"
-- ("hello",())
--
-- >>> runLog $ foreach_ (tell . Product) [1..5]
-- (Product {getProduct = 120},())
foreach_ :: Applicative f => (a -> f b) -> [a] -> f ()
foreach_ _ [] = pure ()
foreach_ f (x:xs) = f x *> foreach_ f xs

-- | Write a function that uses 'Log' to count the number of elements in a list satisfying a given
-- predicate.
--
-- >>> count (odd . length) ["foo", "bar", "baz", "quux"]
-- 3
count :: (a -> Bool) -> [a] -> Int
count p xs = getSum $ (fst . runLog) $ foreach_ go xs
  where go x = if p x then tell $ Sum 1 else tell $ Sum 0

-- | Last chunk of problems, which I think we'll discuss in a bit more depth.  I'm going to give you
-- a new type for which to instantiate 'Applicative', and a couple functions (like 'ask' and 'tell')
-- for interacting with it.  Play around with it and see if you can come up with a way to
-- characterize it.  Obviously, if you already know the answer, don't spoil it for those that are
-- seeing it for the first time.  This is tricky.  Don't worry if it doesn't gel right away (or even
-- at all).

newtype Clog s a = Clog { runClog :: s -> (s, a) }

-- | Instantiate 'Functor' for 'Clog'.  It's like 'Conf', but you have to work harder for it.
instance Functor (Clog s) where
  fmap f cs = Clog (\s -> fmap f ((runClog cs) s))

-- | Instantiate 'Applicative' for 'Clog'.  No need for the 'Monoid' constraint you needed for
-- 'Log'.
instance Applicative (Clog s) where
  pure x = Clog (\s -> (s, x))
  Clog (sab) <*> Clog (sa) = Clog (\s -> go (sab s))
    where go (s', ab) = fmap ab (sa s')

get :: Clog s s
get = Clog (\s -> (s,s))

modify :: (s -> s) -> Clog s ()
modify f = Clog (\s -> (f s, ()))

-- | There's usually a put, here's an implementation for you.
put :: s -> Clog s ()
put = modify . const

-- | If your instance, and 'get' and 'modify' work, this should be an implementation of factorial.
weirderFactorial :: Int -> Int
weirderFactorial x = fst $ runClog (go x) 1
  where go 0 = get
        go n = modify (*n) *> go (n - 1)