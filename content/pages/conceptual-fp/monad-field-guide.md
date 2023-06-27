------------------------
title: 'Monads: A Field Guide'
------------------------

Although we have already discussed what a monad is in general, the actual semantics---what it means for a given type to be a monad---depends on the type itself. Here we'll go over a few of the more common examples of monadic types, how their `Monad` instances are defined, and how they are used.

We are focusing on monads here, but for reference we will define the entire `Monad` type ancestry (i.e. `Functor`, `Applicative`, and `Monad`) together.

## IO

The `IO` monad is perhaps the most (and least) boring of all; it really just wraps a value that can be chained together with other `IO` operations with no actual semantics beyond interacting with the outside world of side effects. By definition its implementation is language-specific, so we will skip right on past to its more language-agnostic relatives.

## Optional

Good old `Optional` is a `Monad` used for chaining computations that may or may not have a result; after the first one that doesn't, none of the following computations run.

    data Optional a = Just a | Nothing

    instance Functor Optional where
        map f (Just a)  = Just (f a)
        map f (Nothing) = Nothing

    instance Applicative Optional where
        pure = Just

        (Just f) <*> (Just x) = Just (f x)
        Nothing  <*> _        = Nothing
        _        <*> Nothing  = Nothing

    instance Monad Optional where
        Just a  >>= f = f a
        Nothing >>= _ = Nothing

Here's what it looks like if you need to chain a bunch of `Optional` results together without monadic operations:

    foo : Optional a
    bar : a -> Optional b
    baz : b -> Optional c

    resultPlain = case foo of
        Just a  -> case bar a of
            Just b  -> baz b
            None    -> None
        None    -> None

The repeated case matching can be replaced by a sequence of monad binds:

    resultMonadic = foo >>= bar >>= baz

That looks much neater! If you check the definition of the `Optional` monad, you can see how the null-checking structure is built in to the definition of `>>=`, which is how we are able to skip so much repetition with monadic code.

## List

The `List` monad has a different meaning entirely, which is somewhat suprising since there is a tendency to believe that two instances of the same class should have similar semantics. That is true, in a sense, but the similarities are somewhat hard to spot. Here is how it is defined:

    data List a = [] | a :: List a      -- empty list, or head + tail

    instance Functor List where
        map f []        = []
        map f (x :: xs) = f x :: (map f xs)

    instance Applicative List where
        pure x = x :: []                                -- a list with one element; the tail is empty

        []        <*> _         = []
        _         <*> []        = []
        (f :: fs) <*> xs = map f xs ++ (fs <*> xs)      -- map each function over each argument and concat them all

    instance Monad List where
        []        >>= _ = []
        (x :: xs) >>= f = f x ++ (f >>= xs)             -- concatenate results for each argument

To make sense of this, let's start with `<*>`. What is its type signature for lists?

    (<*>) : [a -> b] -> [a] -> [b]

In other words, it takes a list of functions, and a list of arguments, and returns a list of results. This is why we have to do a lot of list concatenation: the obvious answer to what that type signature means is "apply each function to each possible argument."

Now for bind:

    (>>=) : [a] -> (a -> [b]) -> [b]

This takes a list of arguments, and a function from *one* argument to a list of results, and returns a list of results. Thus, the meaning of the list monad is to chain a bunch of functions that produce multiple results into one operation that produces *a lot* of results!

It may seem surprising that the same typeclass that gave us free null checks with `Optional` gives us this weird concatenation behavior with `List`s. But the main point is that in both cases, we are chaining function results together within a particular "container" type. How we interpret that chaining behavior just depends on what the type of container we're using.

Using the `List` monad is equivalent to repeated calls to `map` and `flatten`:

    foo : [a]
    bar : a -> [b]
    baz : b -> [c]

    flatten : [[x]] -> [x]
    flatten []          = []
    flatten (xs :: xss) = xs ++ flatten xss

    resultPlain = flatten (map baz (flatten (map bar foo)))

    resultMonadic = foo >>= bar >>= baz

## Either

The `Either` type is another odd one. It takes two type parameters:

    data Either a b = Left a | Right b

From this it should be clear that it simply represents a value that can be one of two possible types. The weirdness comes when we realize that things like `Functor` can only map over one type parameter ^[If you really need to be able to map over both, there is a `Bifunctor` typeclass that will do that for you. We will leave that for [Advanced Typeclasses][advanced-typeclasses].]:

    instance Functor (Either a) where
        map f (Left e)  = Left e
        map f (Right a) = Right (f a)

    instance Applicative (Either a) where
        pure x = Right x

        (Right f) <*> (Right x) = Right (f x)
        (Left e)  <*> _         = Left e
        _         <*> (Left e)  = Left e

    instance Monad (Either a) where
        (Right x) >>= f = f x
        (Left e)  >>= _ = Left e


If you work out the type signature for `map` over `Either a b`, you'll find that it looks like

    map : Either a b -> (b -> c) -> Either b c

This is why we can't do anything with a `Left` value other than leave it alone. All of this is backstory for the main point, which is that `Either` is generally used for operations that can "error out", where a `Left` value wraps an error and a `Right` value wraps a successful result.

The non-monadic version is rather like `Optional`, where we have to do a bunch of nested pattern matching to "short-circuit" the computation when a `Left` value appears.

    foo : Either err a
    bar : a -> Either err b
    baz : b -> Either err c

    resultPlain = case foo of
        Right a -> case bar a of
            Right b -> baz b
            Left e  -> Left e
        Left e  -> Left e

    resultMonadic = foo >>= bar >>= baz

## Reader

`Reader e a` is the type of computations that all need access to some kind of shared configuration or other environment `e` and produce a result of type `a`. This is just another way of saying it's a synonym for functions from `e` to `a`:^[Haskell programmers may find my use of `type` here somewhat unsettling, because in that language this would have to be defined in a `newtype` in order to allow it to get its own typeclass instances. Here, however, we are not restricted by the need to make a compiler understand our meaning, so I am doing away with all of that extra effort of unwrapping and rewrapping newtype instances, with e.g. `runReader` for the sake of human readability.]

    type Reader e a = (e -> a)

    instance Functor (Reader e) where
        map = (.)                   -- function composition!

    instance Applicative (Reader e) where
        pure = const                -- const x makes a "constant function" that always returns x

        f <*> g = \e -> f e (g e)   -- thread the environment to both Readers

    instance Monad (Reader e) where
        f >>= g = \e -> g (f e) e   -- thread the environment through the input and the output

Those definitions are really weird, and to be honest I had to work them out via their type signatures. This is an excellent exercise in programming with types, so let's go over how that works!

First, for `map`, we should have

    map : (a -> b) -> Reader e a -> Reader e b

which is to say,

    map : (a -> b) -> (e -> a) -> (e -> b)

This is how we know that this is the same as function composition - it's the same type signature, and that's really the only way to achieve this type in general!

Now for `pure`, we have

    pure : a -> (e -> a)

This says "give me an `a`, and I'll give you a function that returns a `a` for any input `e`." Once again, there's only one option, which is the `const` function:

    const : a -> (e -> a)
    const x = \_ -> x

Now for a more interesting one, we have applicative apply:

    (<*>) : Reader e (a -> b) -> Reader e a -> Reader e b

which resolves to

    (<*>) : (e -> a -> b) -> (e -> a) -> (e -> b)

We know that the end result needs to be a function of `e`, so we'll start there:

    f <*> g = \e -> ???

We can plug that `e` into `g` to get an `a`, and then we can get the `b` we are looking for by plugging both of those into `f`:

    f <*> g = \e -> f e (g e)

Hopefully the meaning of this is starting to become clear, in the context of computations that refer to a common environment `e`. Here `f` is a function that takes the environment *and another argument*, and `g` is a function that takes the environment and produces a value that can be plugged into `f`. All we had to do was figure out how to socket all these things into the right places to achieve the type we wanted.

Finally, monad bind:

    (>>=) : Reader e a -> (a -> Reader e b) -> Reader e b

i.e.

    (>>=) : (e -> a) -> (a -> e -> b) -> (e -> b)

Once again, we start with a function from `e`:

    f >>= g = \e -> ???

Now, however, we're a little backwards from what we used for `(<*>)`. We plug the `e` into `f` to get an `a`, and then we can plug *that* into `g` along with `e` (again) to produce the `b`:

    f >>= g = \e -> g (f e) e

The point of the `Reader` monad is to save on constantly passing the environment down into sub-steps of a larger computation, similarly to how it saves us from having to constantly pattern-match on `Optional` cases:

    foo : Reader e a
    bar : a -> Reader e b
    baz : b -> Reader e c

    resultPlain = baz (bar (foo e) e) e

    resultMonadic = (foo >>= bar >>= baz) e

In the monadic version, we only needed to explicitly pass the `e` parameter once! Of course, in this simple example it hardly made a difference, but for more complex systems, this can be a huge saving in readability.

## Writer

`Writer o a` is used for operations that successively append to a stream of output data `o`, such as a record of which steps were taken or an incremental parser, in addition to the result type `a` of each step. It can easily be implemented as a tuple:

    type Writer o a = (o, a)

    instance Functor (Writer o) where
        map f (o, a) = (o, f a)     -- map the value, leave the accumulated value alone

The `Functor` instance is easy enough, but we need something extra for `Applicative` and `Monad`; we're missing a way to start the accumulated output with `pure`, and add them together across each step in our computation with `(<*>)`. Luckily, we already have a way to define those two operations: the output just needs to be a `Monoid`!

    instance (Monoid o) => Applicative (Writer o) where
        pure x = (empty, x)         -- start with an empty accumulator

        (o1, f) <*> (o2, x) = (o1 <> o2, f x)              -- concat accumulators, apply the function to the value

    instance (Monoid o) => Monad (Writer o) where
        (o1, x) >>= f = let (o2, y) = f x in (o1 <> o2, y) -- concat accumulators, carry forward the output value

Here is how a chain of `Writer` operations might look without and with monadic operations. For concreteness, I will actually use a list of `String` values as the accumulator type:

    aValue : a
    bValue : a -> b
    cValue : b -> c

    foo : Writer [String] a
    foo = (["called foo"], aValue)

    bar : a -> Writer [String] b
    bar a = (["called bar"], bValue a)

    baz : b -> Writer [String] c
    baz b = (["called baz"], cValue b)

    resultPlain = let
        (o1, a) = foo
        (o2, b) = bar a
        (o3, c) = baz b
        in (o1 ++ o2 ++ o3, c)

    resultMonadic = foo >>= bar >>= baz

In both cases, the accumulated output acts like a log indicating the order that the functions were called: `["called foo", "called bar", "called baz"]`.

## State

Our final example of a generic monad is `State`: the type of computations that update some kind of running "state" in addition to producing a output. This can be represented as a function from an initial state to a tuple of the final state and the output:

    type State s a = s -> (s, a)

    instance Functor (State s) where
        map f st = \s -> let (s, x) = st s in (s, f x)      -- leave the state unchanged

    instance Applicative (State s) where
        pure x = \s -> (s, x)           -- leave the state unchanged and just return the given value

        stf <*> stx = \s -> let
            (s1, f) = stf s             -- run the first computation to get the function and a new state
            (s2, x) = stx s1            -- run the second, with the updated state from the first, to get the value and another new state
            in (s2, f x)                -- put them together with the final state

    instance Monad (State s) where
        st >>= f = \s -> let
            (s1, x) = st s              -- run the first thing to get a new state and its value
            in f x s1                   -- run the bound function with that value, then plug in the new state

There isn't much trickiness here; we just need to make sure we're carrying the intermediate state changes forward through each step of the computation.
