-- Today's homework: Functors!
import Data.Semigroup(Any(..), Sum(..))

-- Functors are great.  You're already familiar with several types that have instances of 'Functor'.
-- 'Maybe', '[]', 'Either' (about which we haven't spoken much, but we probably should), etc.  Much of
-- this homework will be instantiating 'Functor' for all of these types (or, rather, newtype wrappers
-- around them).

-- There are many metaphors for what functors "really are".  I'm going to pick one of them that I
-- rather like: Given a value of type @a@ and a functor @f@, a value of type @f a@ represents "@a@, but
-- in a world with @f@-effects".

-- As you'll recall, "effect" refers to features like IO, failure, non-determinism, etc.  In fact,
-- we'll start thinking of "effects" as corresponding to certain functors (the ones with 'Applicative'
-- instances---we'll deal with that next week).  That is, rather than worrying about what effect '[]'
-- corresponds to, we might just assume that if @f@ is a functor, then it characterizes the @f@ effect,
-- whatever that happens to be.

-- So, here's the mental model.  We've been writing programs in Haskell for a while---we'll think of
-- the "world of Haskell" as any of the values we have seen (or might see in the future).  So say we
-- have an 'Int' named @x@ in the "world of Haskell".  Then we'll think of @Just x@ as an 'Int' in the
-- world of "Maybe Haskell"---Haskell, but with the 'Maybe' (failure) effect.  (Don't worry if this is
-- all too esoteric---come back and read this again after going through the problems, and always feel
-- free to ask me any questions you might have whenever.)  Notice how 'Maybe' tranforms __any__ type
-- into that-type-but-with-failure.  'Maybe' (or '[]' or whatever) transforms ALL OF HASKELL into
-- Haskell-but-with-failure (or whatever effect).  This perspective will be very helpful when we get to
-- monads.

-- The 'Functor' class looks like this:

-- @
--    class Functor f where
--      fmap :: (a -> b) -> f a -> f b
-- @

-- For example: @fmap length (Just [1..10])@ is @Just 10@.

-- The laws for 'Functor' are:

-- fmap id x         = x              (identity)
-- fmap f (fmap g x) = fmap (f . g) x (composition)

-- These equations are sometimes written point-free:

-- fmap id         = id
-- fmap f . fmap g = fmap (f . g)

-- Let's think about what these laws mean.  The identity literally says "mapping the identity function
-- is the same as not mapping at all".  Intuitively, it tells you that the implementation of 'fmap'
-- for a 'Functor' can't change the structure of the data type with the instance, because 'id' can't
-- change any of the @a@'s it encounters, and so the 'fmap' must leave the structure of the data unchanged
-- as well.  @fmap f (Just x) = Nothing@ has the right type, but it violates the identity law.

-- Similarly, the composition law literally says "mapping @g@ and then mapping @f@ is the same as mapping
-- @g@-and-then-@f@ once".  (You could think of this as a form of loop fusion).  Informally, I'd like to
-- interpret it as saying that "function composition in the world of Haskell-with-some-effect works just
-- like function composition in the normal Haskell world".  Mapping the Haskell-world composition @f . g@
-- is just like mapping @g@ and then mapping @f@.

-- One way to see this this perspective that 'Functor' lets us work in Haskell-but-with-some-effect is to
-- look at the type of 'fmap':

-- @
--     fmap :: Functor f => (a -> b) -> f a -> f b
-- @

-- You can read that as saying "for a functor f, fmap takes a function from a to b, and an @f a@, and makes
-- an @f b@".  But if you add some extraneous parentheses:

-- fmap :: Functor f => (a -> b) -> (f a -> f b)

-- It shows us more clearly that 'Functor' turns regular Haskell-world functions @a -> b@ into @f@-world
-- functions @f a -> f b@.  Indeed, there's an infix operator that's an alias for fmap:

-- @
--     (<$>) :: Functor f => (a -> b) -> f a -> f b
--     (<$>) = fmap
-- @

-- The @$@ is a visual queue that there's a sense in which 'fmap' behaves like the '($)' we know and love:

-- @
--    ($)   ::              (a -> b) ->   a ->   b
--    (<$>) :: Functor f => (a -> b) -> f a -> f b
-- @

-- We see 'fmap' as effectively just applying your @a -> b@ "under" the @f@.

-- You may have noticed that this file has an @.lhs@ extension, rather than our normal @.hs@.  You may
-- also have noticed that there are no comment characters in this file.  This is a so-called "literate
-- Haskell" file, which ghc should understand perfectly well.  Code will appear prefixed by "bird
-- tracks", which are just @>@ characters.  (https://wiki.haskell.org/Literate_programming#Bird_Style).


-- Finger Exercises:

-- Implement 'Functor' for each of the following types.  I started out just making newtype wrappers for
-- standard Haskell types, but that got annoying real fast, so we're back in 'Option' and 'List' land
-- for a bit.

newtype Identity a = Identity a -- this type is in 'Data.Functor.Identity'
  deriving (Eq, Show)

-- The 'Identity' functor (and identity monad and identity just about everything) behaves as if it
-- weren't there.  Believe it or not, this turns out to be useful.

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

-- Now your turn---implement 'Functor' for each of the types below.

data Option a = None | Some a
  deriving (Eq, Show)

instance Functor Option where
  fmap f (Some x) = Some $ f x
  fmap _ None     = None

data List a = Nil | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a xs) = Cons (f a) (fmap f xs)

data Tree a = Leaf | Branch a (Tree a) (Tree a)
  deriving (Eq, Show)

instance Functor Tree where
  fmap _ Leaf = Leaf
  fmap f (Branch a ta ta') = Branch (f a) (fmap f ta) (fmap f ta')

data Pair a b = Pair a b
  deriving (Eq, Show)

instance Functor (Pair a) where
  fmap f (Pair a b) = (Pair a (f b))

-- Now, for the types below, instantiate 'Functor' if you can, IF NOT SAY WHY NOT.  Try your best to do
-- the implementation without googling or asking your IDE---it's a learning experience.

data Power a = Power Int a
  deriving (Eq, Show)

instance Functor Power where
  fmap f (Power n a) = Power n (f a)

newtype From r a = From (r -> a)

instance Functor (From r) where
  fmap f (From g) = From (f . g)

newtype To r a = To (a -> r)
-- To does not admit a functor instance, since a is contravariant



-- Assume 'OrderedList' has the same semantics as in the last homework.  Feel free to copy in any functions
-- from there that you like.

newtype OrderedList a = OrderedList { getOrderedList :: [a] }
  deriving (Eq, Show)

-- Ordered list cannot admit a functor instance without breaking the semantics.
-- each ordered list needs an Ord instance for the items inside. If they're mapped to different
-- types, fmap would need to add a constraint on b which is not present in Functor's fmap


-- I was about to post this homework, and I realized I didn't have you write a function using 'Functor', just
-- instantiate it a bunch of times.  Here's a stupid function to implement: Given an alist with some key type
-- and some Foldable for values, say whether the length of the value at a particular key is an even number, giving
-- back 'Nothing' when there is no such key.  Here's an example, 'cause that's super weird:

-- @
--     stupid "thingy" [("thingy", "wingy"), ("stuff", "things")]
--     >>> Just False

--     stupid "stuff" [("thingy", "wingy"), ("stuff", "things")]
--     >>> Just True

--     stupid "missing" [("thingy", "wingy"), ("stuff", "things")]
--     >>> Nothing
-- @

-- Now, here's the thing: solve this problem calling 'fmap' three times (with non 'id' arguments...).  Oh, and
-- you can use @even :: Int -> Bool@ which says whether a number is even and is in scope already, as well as
-- @lookup :: Eq k => k -> [(k,v)] -> Maybe v@, which is also in scope.

len :: Foldable t => t a -> Int
len xs = getSum $ foldMap (\_ -> Sum(1)) xs

stupid :: (Eq k, Foldable t) => k -> [(k,t a)] -> Maybe Bool
stupid key xs = fmap getAny $ foldMap go $ fmap (fmap (Any . even . len)) xs
  --where go :: (k, Bool) -> Maybe Bool
  where    go (k, isEven) = if k == key then Just isEven else Nothing
