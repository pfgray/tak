import Algebra.Lattice
import Algebra.Lattice (BoundedJoinSemiLattice, BoundedMeetSemiLattice)
{-|

This week (and probably next week as well) we'll be talking about type classes---both the language
feature and several important concrete examples.

Many of the examples in this homework already exist in some library or another.  Do your best NOT to
try and pull them in. Also, no credit is awarded for instances created using @deriving@.

For all the "Instantiate $CLASS for $TYPE" problems below, your instances should be lawful and
non-stupid.  I'll remind you of the relevant laws below.  "Non-stupid" means your instances should
also be somewhat actually reasonable.  So, @instance Eq Foo where (==) _ _ = True@, while lawful, is
stupid (presuming the hypothetical @Foo@ is inhabited by more than one value).

Lastly, please actually code each instance up fresh.  There's going to be a bunch of repetition, but
please resist the urge to copy-paste.  I really believe the best way to get comfortable writing
Haskell is actually to write it.  Even if it's a little boring, getting comfortable inputting
Haskell syntax takes practice and repetition.

|-}

-- | This 'Option' type is basically the same as 'Maybe', except it doesn't already have any
-- instances.
data Option a = None | Some a
  deriving (Show)

-- * Finger Exercises

{-|

The laws for 'Eq' are:

for every x, y and z

(reflexivity):  x == x
(symmetry):     x == y implies y == x
(transitivity): x == y and y == z implies x == z

The laws for 'Ord' are:

for every x, y and z

(reflexivity):  x <= x
(antisymmetry): x <= y and y <= x implies x == y
(transitivity): x <= y and y <= z implies x <= z

|-}


-- | Instantiate 'Eq' for 'Option'

-- YOUR INSTANCE HERE
instance (Eq a) => Eq (Option a) where
  Some x /= Some y = x /= y
  None   /= None   = False
  _      /= _      = True

-- | Instantiate 'Ord' for 'Option'.  'None' is always less than @'Some' x@.

-- YOUR INSTANCE HERE
instance (Ord a) => Ord (Option a) where
  compare (Some x) (Some y)  = compare x y
  compare None     (Some _) = LT
  compare (Some _) None   = GT
  compare None     None   = EQ

-- * Monoids and Semigroups

{-|

'Monoid' is one of my favorite type classes.  Monoids are very simple, and have a broad range of
applications.  Back in the day, the class was defined as

class Monoid m where
  mappend :: m -> m -> m
  mempty  :: m

Reading that in semi-English: a type @m@ is a Monoid when we have a binary operation @mappend@ on
@m@ and a distinguished value @mempty@ of type @m@.

The laws for Monoids are:

(associativity): mappend x (mappend y z) = mappend (mappend x y) z
(identity):      mappend x mempty = mappend mempty x = x

(I'm using a single @=@ above so as not to imply that monoids must be 'Eq').

You're familiar with many examples of monoids.  Integers are a monoid if @mempty = 0@ and @mappend =
(+)@.  They're also a monoid if @mempty = 1@ and @mappend = (*)@.  Indeed, there are other monoids
you might wish to use over the integers (which will present a problem we'll discuss in a moment).
I'd give a bunch more examples here, but you'll be implementing many of them below.

The class declaration above is a bit of a fib for two reasons.  The first reason is that 'Monoid'
also provides @mconcat :: [m] -> m@ with a default implementation that you're allowed to override if
you think you can do better than @foldr mappend mempty@.  The second is that over the course of
years, we realized that there are some very important examples of things that are very-nearly
Monoids, but lack an identity element ('mempty').  These are called 'Semigroup's, and their class
declaration looks like this:

class Semigroup a where
  (<>) :: a -> a -> a

NB: This is also a lie, a couple methods similar to 'mtimes' also exist, but they have default
implementations and I don't want to talk about why you might want to override them just now.  (Feel
free to go look at 'sconcat' and 'stimes' when you're done with these exercises, but they aren't
relevant to the homework, so I'm ignoring them for now).

I like to pronounce @(<>)@ "smash", as in "smash these two values together".  So the actual class
declaration for Monoid is:

class Semigroup m => Monoid m where
  mappend :: m -> m -> m
  mappend = (<>)

  mempty :: m

  mconcat :: [m] -> m
  mconcat = foldr mempty mappend

I know this introductory comment in getting long, but I have one more thing to cover, and then we're
on to the code.  I mentioned above that there are actually several different interpretations of the
Integers as a Monoid.  This is a problem for Haskell, because there can be at most one instance of a
type class for a given type.  So which is the "right" monoid to use for Integers?  There's really no
correct answer.  For some types, there really is a "right" choice---sometimes it will be called
"canonical", in that annoyingly Haskelly sense of "there's only one thing this could possibly be
that's law-abiding and not-stupid".

When we're confronted with a type that doesn't have a canonical interpretation for a type class (or
indeed, if we just wish use that type with a different interpretation for some reason), we use a
feature of Haskell we briefly discussed in class called @newtype@.  A @newtype@ is just like a
@data@ declaration, except that you can only have one constructor containing a single type.  This is
because the purpose of @newtype@ is to introduce a new type into the program without creating a new
representation of that type.  Don't worry if that sounds too magic.  We can discuss the finer points
of it later.  The important takeaway for now is that since a @newtype@ is a new type (not an
existing one), it can have different type class instances than the type it wraps.  Hopefully this
will all become plain momentarily, but don't hesitate to ask if it's confusing.

|-}

-- | I'll start you off with an example: for booleans, both @(&&)@ and @(||)@ are monoids, so
-- there's clearly no canonical instance of 'Monoid' for 'Bool'.  Here's how we use @newtype@ to
-- deal with that.  (Notice I'm using the record-accessor syntax, which I believe we've seen,
-- because it makes using these wrappers much nicer).

newtype All = All { getAll :: Bool }
  deriving Show -- you can derive 'Show', don't get more clever unless I say so :)

-- | Remember that because 'Semigroup' is a superclass of 'Monoid', you need to provide an instance
-- for both.
instance Semigroup All where
  All x <> All y = All (x && y)

-- | We're happy to use the default implementation of @mappend = (<>)@.  I don't actually know of
-- any particular case where you'd want to specialize 'mappend', but it has to be here for
-- historical reasons.  I also **deeply** hate the name "mappend" (and "mempty" as well), so I don't
-- mind typing it less.
instance Monoid All where
  mempty = All True

newtype Any = Any { getAny :: Bool }
  deriving Show

instance Semigroup Any where
  Any x <> Any y = Any (x || y)

instance Monoid Any where
  mempty = Any False

-- | Here's a wrapper for taking the sum of 'Int's.
newtype IntSum = IntSum { getIntSum :: Int }
  deriving Show

instance Semigroup IntSum where
  IntSum x <> IntSum y = IntSum (x + y)

instance Monoid IntSum where
  mempty = IntSum 0

{-|

Try a few of these in the repl.

>>> getAny (Any True <> Any False)

>>> getAll (All True <> All False)

>>> getIntSum (IntSum 4 <> mempty)

Notice that without those accessors ('getAny', 'getAll', etc.) you get a value wrapped in its
newtype.

|-}

-- | Ok, your turn.  Let's do products of 'Int's, and then we'll make things a bit more interesting.
--
-- >>> getIntProduct (IntProduct 2 <> IntProduct 9)
-- 18
newtype IntProduct = IntProduct { getIntProduct :: Int }
  deriving Show

instance Semigroup IntProduct where
  IntProduct x <> IntProduct y = IntProduct (x * y)

instance Monoid IntProduct where
  mempty = IntProduct 1


-- | Great.  But pretty boring.  There's no reason why we need to restrict our arithmetic to just
-- integers.  Instantiate Semigroup and Monoid for this new, polymorphic Sum wrapper.  (Hint: this
-- is trickier than the examples I've done---we've covered what you need in class, but if you
-- cargo-cult the answer here, you should get a compile error).
newtype Sum a = Sum { getSum :: a }
  deriving Show

instance (Num a) => Semigroup (Sum a) where
  Sum x <> Sum y = Sum (x + y)

instance (Num a) => Monoid (Sum a) where
  mempty = Sum 0

-- | Same deal with products...
newtype Product a = Product { getProduct :: a }
  deriving Show

instance (Num a) => Semigroup (Product a) where
  Product x <> Product y = Product (x * y)

instance (Num a) => Monoid (Product a) where
  mempty = Product 1

-- | A quick break from Monoids just to give a different example of using newtypes to control the
-- behavior of types, even if they already have an instance.  The purpose of the 'Down' newtype is
-- to reverse the (already-defined) ordering on a type.  So if @3 < 4@, then @Down 4 < Down 3@.
-- Instantiate 'Ord' for 'Down'.
--
-- >>> max 3 4
-- 4
-- >>> getDown (max (Down 3) (Down 4))
-- 3
newtype Down a = Down { getDown :: a }
  deriving Show

instance (Eq a) => Eq (Down a) where
  Down x /= Down y = x == y

instance (Ord a) => Ord (Down a) where
  compare (Down x) (Down y) = case (compare x y) of
    GT -> LT
    LT -> GT
    EQ -> EQ

-- | Let's try appending lists.  Appending is really the only reasonable choice of monoid for lists,
-- so we don't need a newtype wrapper for it.  We'll use our own version of lists because the
-- instances for lists already exist. Instantiate Semigroup and Monoid for the 'List' type below.

data List a = Nil | Cons a (List a)
  deriving Show

instance Semigroup (List a) where
  Cons x xs <> ys  = Cons x (xs <> ys)
  Nil       <> ys  = ys

instance Monoid (List a) where
  mempty = Nil

-- | It's probably getting annoying to always have to write a separate Semigroup instance when you
-- know you have a Monoid.  The only reason that work is justified is because there are many
-- examples of Semigroups that simply aren't Monoids.
--
-- The 'First' Semigroup always takes the first of @(<>)@ arguments.  It's not particularly useful
-- on its own, but you'll see why trivial-seeming things like 'First' can be useful next week when
-- we start talking about other abstractions that work for *any* Monoid (or Semigroup).  Implement
-- Semigroup for 'First'.
--
-- >>> getFirst (First "foo" <> First "bar" <> First "baz")
-- "foo"
newtype First a = First { getFirst :: a }
  deriving Show

instance Semigroup (First a) where
  fx <> _ = fx

-- | You knew it was coming...  While you're writing out this instance, perhaps meditate on why
-- First and Last aren't Monoids.
newtype Last a = Last { getLast :: a }
  deriving Show

instance Semigroup (Last a) where
  _ <> ly = ly

-- | Further yet more.  For the following, implement Semigroup, and Monoid **if you can**.  Some of
-- these have sensible Monoid instances, some don't.
--
-- Smash two values together for their maximum
newtype Max a = Max { getMax :: a }
  deriving Show

-- For Max/Min, I originally did this with Ord,
-- however I don't think there's a top/min value,
-- so I couldn't construct a Monoid,
-- I've known a little bit about lattices, and wanted
-- to try them out here

--instance (Ord a) => Semigroup (Max a) where 
--  Max a <> Max b = Max (a `max` b)
instance (Lattice a) => Semigroup (Max a) where
  Max a <> Max b = Max (a /\ b)
instance (BoundedJoinSemiLattice a) => Monoid (Max a) where
  mempty = Max bottom

-- | ...and for their minimum
newtype Min a = Min { getMin :: a }
  deriving Show

--instance (Ord a) => Semigroup (Min a) where 
--  Min a <> Min b = Min (a `min` b)
instance (Lattice a) => Semigroup (Min a) where
  Min a <> Min b = Min (a \/ b)
instance (BoundedMeetSemiLattice a) => Monoid (Min a) where
  mempty = Min top

-- | Turns out you can come up with something for this.  The word "endo" means "in", in the sense of
-- "endoskeleton" or "endoscopy".  This newtype is for endofunctions.
newtype Endo a = Endo { getEndo :: a -> a }

instance Semigroup (Endo a) where
  Endo f <> Endo g = Endo (f . g)

instance Monoid (Endo a) where
  mempty = Endo id

-- | Let's do a couple of tuples.  These instances exist already for the built-in tuples, so we'll
-- use our own versions of the types.  Implement Semigroup-and-possibly-Monoid for:

-- | This is really just standing in for @(a,b)@.
data Pair a b = Pair a b
  deriving Show

instance (Semigroup a, Semigroup b) => Semigroup (Pair a b) where
  Pair x y <> Pair x' y' = Pair (x <> x') (y <> y')

instance (Monoid a, Monoid b) => Monoid (Pair a b) where
  mempty = Pair mempty mempty

-- | This is really just standing in for @()@
data Unit = Unit

instance Semigroup Unit where
  _ <> _ = Unit
instance Monoid Unit where
  mempty = Unit

-- | This one is pretty subtle; not complicated, just subtle.  Remember 'Option' is the same as
-- 'Maybe', just being used here for examples.  Instantiate Monoid for 'Option'---The identity will
-- be 'None'.  We need to use this below, so give it some thought.  Ask me if you have trouble, and
-- feel free to check with me when you think you have it.

-- INSTANTIATE MONOID FOR 'Option' HERE!
instance (Semigroup a) =>  Semigroup (Option a) where
  Some x <> Some y = Some (x <> y)
  None   <> Some y = Some y
  Some x <> None   = Some x
  None   <> None   = None

instance (Semigroup a) => Monoid (Option a) where
  mempty = None

-- * Let's use these things a bit

-- | 'Dual' is a peculiar little wrapper that behaves like it's underlying Monoid, but flips the
-- order that it applies its arguments.  This doesn't matter when the monoid in question is
-- symmetric (we defined "symmetry" above), but we've already seen that not all Monoids (or
-- Semigroups) are.  I'll give you the implementation, your task will be to use it in a moment.
--
-- (You may notice that all my newtypes follow an extremely predictable form.  This is not a
-- requirement of the language, it's just a pretty common convention.)
--
-- >>> "foo" <> "bar"
-- "foobar"
---
-- >>> getDual (Dual "foo" <> Dual "bar")
-- "barfoo"
newtype Dual a = Dual { getDual :: a }
  deriving Show

instance Semigroup a => Semigroup (Dual a) where
  Dual x <> Dual y = Dual (y <> x)

-- | Bonus question to think on: why is 'mempty' the same (modulo the wrapper) for @m@ and @Dual m@?
instance Monoid m => Monoid (Dual m) where
  mempty = Dual mempty

-- | For the following examples, this function will be useful.
mapOption :: (a -> b) -> Option a -> Option b
mapOption f (Some x) = Some $ f x
mapOption _ None     = None

-- | Get the first element of the list, if it exists, using 'First', 'Option', 'mconcat', and 'map'.
-- We haven't talked about 'map', but I think you all know what it does.  It's type is:


--  @map :: (a -> b) -> [a] -> [b]@
first :: [a] -> Option a
first xs = mapOption getFirst $ mconcat $ map (Some . First) xs

-- | Get the final element of the list, if it exists, using only 'First', 'Option', 'mconcat', 'map'
-- and 'Dual'.
final :: [a] -> Option a
final xs = mapOption (getFirst . getDual) $ mconcat $ map (Some . Dual . First) xs

-- | Get the average of a list of Doubles, using 'Pair', 'mconcat', 'map' and the various other
-- wrappers we've implemented today.
average :: [Double] -> Option Double
average xs = mapOption divide $ mconcat $ map toAvgPair xs
  where toAvgPair x = Some $ Pair (Sum 1) (Sum x)
        divide (Pair (Sum count) (Sum total)) = total / count



-- some helpers for testing the lattice stuff
data Cadence =
  Never
  | Hourly
  | Daily
  | Weekly
  | Monthly
  | Quarterly
  | Biannually
  | Annualy
  | Biennial
  deriving (Show, Eq, Ord)

instance Lattice Cadence where
  x /\ y = x `max` y
  x \/ y = x `min` y

-- hmm... BoundedMeetSemiLattice is custom, but I bet there's
--   machinery to derive custom instances. This would be preferable
--   in this case, since what if someone adds a cadence to the end?
instance BoundedMeetSemiLattice Cadence where
  top = Biennial

instance BoundedJoinSemiLattice Cadence where
  bottom = Never