------------------------
title: Intermediate Typeclasses
------------------------

So far, the typeclasses we've seen have obvious analogues in non-functional languages too. This section will look at a few of the typeclasses that are particularly important to functional programming design.

## Functor

The `Functor` typeclass is for data types that can be treated like containers whose elements can be "mapped over." Specifically:

    typeclass Functor f
        map : (a -> b) -> f a -> f b

You might also think about a functor as a way to apply a function *through* a data structure. Lists are a common example; indeed, many object-oriented languages give a `map` method to their array or list class.

    instance Functor []
        map _ []      = []
        map f (x::xs) = f x :: map f xs

    -- this evaluates to [10, 4]
    someInts = map stringLength ["functional", "peep"]

`Optional` is also a useful `Functor`\:

    instance Functor Optional
        map _ Nothing  = Nothing
        map f (Just x) = Just (f x)

## Foldable

`Foldable` is the typeclass of data structures that can be "folded", accumulating some result at each point:

    typeclass Foldable t
        foldl : (b -> a -> b) -> b -> t a -> b

The `l` at the end of `foldl` indicates that this is a *left fold* ^[This is also called a *left-associative* fold, when there are mathematicians around.]. The words "left" and "right" refer to the head and tail of a list, respectively; in general, left folds start at the "front" of a data structure (they are *breadth-first*), and right folds start at the "back" (*depth-first*). What this means is best illustrated by an example. Consider this definition of the `sum` function, which sums a list of integers:

    instance Foldable []
        foldl _ acc []          = acc
        foldl f acc (x :: xs)   = foldl f (f acc x) xs

    sum : [Int] -> Int
    sum = foldl (+) 0

Now let's consider what happens when this function is evaluated. This next bit isn't code; each line represents a step of "unwrapping" the fold until we get to the point where we only have math left to do:

```plain
   sum [1, 2, 4, 8]
   foldl (+) 0 [1, 2, 4, 8]
   foldl (+) (0 + 1) [2, 4, 8]
   foldl (+) ((0 + 1) + 2) [4, 8]
   foldl (+) (((0 + 1) + 2) + 4) [8]
   foldl (+) ((((0 + 1) + 2) + 4) + 8) []
   ((((0 + 1) + 2) + 4) + 8)
```

As you can see, the first thing to be evaluated is $0 + 1$, and we proceed down the list, evaluating the "left-most" operations first. This implies the existence of a *right fold* ^[See [Right Folds from the Left][appendix-foldr] for a generic right-fold, if all we have to start with is a left-fold.]:

    foldrList : (a -> b -> b) -> b -> [a] -> b
    foldrList _ b [] = b
    foldrList f b (x :: xs) = f x (foldr f b xs)

    sumr : [Int] -> Int
    sumr = foldr (+)

When this is evaluated, we get

```plain
sumr [1, 2, 4, 8]
foldr (+) 0 [1, 2, 4, 8]
(1 + (foldr (+) 0 [2, 4, 8]))
(1 + (2 + (foldr (+) 0 [4, 8])))
(1 + (2 + (4 + (foldr (+) 0 [8]))))
(1 + (2 + (4 + (8 + (foldr (+) 0 [])))))
(1 + (2 + (4 + (8 + 0))))
```

Unsurprisingly, now we're starting on the right! This ends up evaluating to the same result, but that is only the case for *associative* operations. You may recall from math class that this has to do with how we group a series of operations; if we just write $0 + 1 + 2 + 4 + 8$ there are five different $+$s that we could choose to evaluate first. Of course, with addition, it doesn't matter; any way we group the operations comes out to the same result. We call functions with this property associative. On the other hand, subtraction is definitely not associative:

```plain
((((0 - 1) - 2) - 4) - 8) = -15
(1 - (2 - (4 - (8 - 0)))) = -5
```

In this case, `foldl` and `foldr` give different results! This isn't actually that big of a deal though---if you know which side you're starting from, you can always define your folding function appropriately (and perhaps reverse your list) in order to get the result you want. It turns out, though, that sometimes it does matter which fold you choose!

The examples above with `(+)` are *reductions*: they collapse the list as they traverse it. Both reductions happen in linear time (since they traverse the input list exactly once), but `foldl` happens in constant space, while `foldr` uses linear space! For very long lists, this can easily overflow the stack. The reason is that when folding from the left, we're keeping a "running total" of the folded value; each rescursive call need not generate its own stack frame, so the fold only needs as much memory as is required to store the result value. On the other hand, folding from the right means that we must traverse the entire list before we can start evaluating stuff, and each time we recurse further into the list, we have to hold on to the current value while we wait for the evaluation to work its way back up the stack!

However, not all folds are reductions, and interestingly, the situation is reversed for non-reductive folds. Consider the two functions below, which implement `map` over a list, one with a left fold and the other with a right fold. You should be able to convince yourself that they both produce the same result as we saw for the `Functor` instance above:

    mapl : (a -> b) -> [a] -> [b]
    mapl f = foldl mapAndAppend [] where
        mapAndAppend ys x = ys ++ [f x]

    mapr : (a -> b) -> [a] -> [b]
    mapr f = foldr mapAndPrepend [] where
        mapAndPrepend x ys = f x :: ys

Note that when we're folding from the left, we put each successive result at the *end* of the new list. Likewise, when we're folding from the right, we start at the end of the list, so we append each result to the head of the new list. Evaluating these as we did before, we get

    mapl stringLength ["Mrs", "Birdy", "says", "peep"]
    -- (((([] ++ [3]) ++ [5]) ++ [4]) ++ [4])

    mapr stringLength ["Mrs", "Birdy", "says", "peep"]
    -- (3 :: (5 :: (4 :: (4 :: []))))

The problem here is that concatenation using `++` runs in time proportional to the length of the left-hand list, and each time we do a concatenation, the left-hand list gets bigger; suddenly our left fold is in *quadratic time*! We would therefore rather choose a right fold for this job, because it allows us to use the constant-time list constructor `::` rather than linear-time concatenation.

Now, depending on your language's evaluation rules, how it implements lists, and particuarly how smart its optimizer is, your mileage may vary. The moral of this story is that you should choose your fold so that reductions are *strict and tail-recursive*, and non-reductive folds build the output structure efficiently, using only constant-time operations (if possible).

## Monoid

First, a warning: monoids are to monads as Java is to JavaScript, so apologies in advance for the similar words. Blame mathematicians again.

Here's the definition of `Monoid`\:

    typeclass Monoid a
        empty : a
        (<>) : a -> a -> a

This can be read a couple of different ways. Usually the one folks see first treats `<>` as an operator for glomming two instances of the monoid together, with `empty` as the "neutral" element; for example, with integers:

    instance Monoid Int as Sum
        empty = 0
        (<>)  = (+)

    sum : [Int] -> Int
    sum xs = foldl (<> using Sum)

Notice that I have named the instance; this can sometimes be useful, because there may be multiple ways for a given data type to implement a typeclass. Such as:

    instance Monoid Int as Product
        empty = 1
        (<>)  = (*)

Each of these specifies a particular way that integers can be stuck together. With these examples handy, we can write down the *monoid laws*:

Associativity
:   `(x <> y) <> z == x <> (y <> z)`

Identity
:   `x <> empty == empty <> x == x`

The requirement that `<>` be associative means that there aren't monoid instances for division or subtraction. (By the way, division has another problem too; `<>` should always be defined for all values, but division by zero isn't defined!)

The other way to interpret a monoid is as a way to choose between two values with `<>`, with `empty` providing a default choice.

    instance Monoid (Maybe a) as First
        empty = Nothing
        
        Just x  <> _        = Just x
        Nothing <> Just x   = Just x
        Nothing <> Nothing  = Nothing

    instance Monoid (Maybe a) as Last
        empty = Nothing
        
        _       <> Just y   = Just y
        Just x  <> Nothing  = Just x
        Nothing <> Nothing  = Just x

Here, the `First` instance always chooses the first non-`Nothing` value it was given; likewise, `Last` always chooses the last.

As a final example, `Bool` also admits two possible monoids:

    instance Monoid Bool as All
        empty = True
        (<>)  = (&&)

    instance Monoid (Maybe a) as Any
        empty = False
        (<>)  = (||)

## Applicative

The extravagantly-named *applicative functor* is, of course, simply a functor that is applicative!

That sounds offensively unhelpful, but interestingly it's one of the more meaningful names for important concepts (looking at you, 'Monad'). To illustrate what it means, let's consider a puzzle. A program has asked the user for two integers, `x` and `y`, but since getting these integers involves communing with the outside world of side effects, they are both of type `IO Int`. Your goal is to add them together. How can we do this?

Unlike most data types, `IO` values cannot be "unwrapped", because that would defeat the purpose of keeping side effects contained. `IO` is a functor, so we can do things like

    x : IO Int
    x = askUserForInt

    y : IO Int
    y = askUserForInt

    z : IO Int
    z = map (*2) x -- double it!

but before you ask, `x + y` doesn't work because `IO Int` is not a number! It's more like a *promise* of a number, and in fact thinking about `IO` like an ES `Promise` or a Java `CompletableFuture` is not a terrible approximation.

Okay fine, it's a trick question, and presumably you have already figured out that the answer has to do with whatever an applicative is. Plain functors simply don't provide enough power to support this sort of operation. Happily, `IO` is an `Applicative`, which gives us access to this gadget:

    liftA2 : (Applicative f) => (a -> b -> c) -> (f a -> f b -> f c)
    -- definition will come in a moment!

    addTwoIOs : IO Int -> IO Int -> IO Int
    addTwoIOs = liftA2 (+)

    addXAndY = addTwoIOs x y -- ta da!

The function `liftA2` takes a pure function of two arguments, and turns it into a function over two Applicative arguments. The term *lift* is one that will occur a lot; it's usually given to a function that takes a "plain" function and transforms it into a "special" one—e.g. *lifting* the humble `(+)` into the exciting world of `IO`. "`liftA`" denotes a lift into Applicatives, and "`liftA2`" indicates that it operates on functions of two arguments; once you get over that hurdle, it's easy enough to construct `liftA`$n$ but usually that's excessive. In fact, you've already seen `liftA1`\: it's just functor `map`!

    liftA1 : (Applicative f) => (a -> b) -> (f a -> f b)
    -- where have I seen this type signature before?

Hopefully that is enough to start shedding light on the name *applicative functor*. Let's look at how it's actually defined.

    typeclass (Functor f) => Applicative f
        pure  : a -> f a
        (<*>) : f (a -> b) -> f a -> f b

The `pure` function lifts a plain value into an applicative. The name is intended to suggest that we're getting "just" that value: no spooky side effects, no accidental emails to scandalize grandma, it's a pure value. For instance, if we didn't want to bother asking the user for numbers (they would probably screw it up anyway), we could just say

    myX : IO Int
    myX = pure 2

    myY : IO Int
    myY = pure 3

The other thing, `(<*>)`, is pronounced "apply", and it takes a lifted single-argument function and applies it to a lifted value. These two things together allow us to define `liftA2`\:

    liftA2 : (Applicative f) => (a -> b -> c) -> (f a -> f b -> f c)
    liftA2 f x y = pure f <*> x <*> y

Which is to say, we lift `f` up into the applicative, (partially!) apply it to `x`, and then finally apply that to `y`. In fact, we could have started with `liftA2` instead:

    (<*>) : (Applicative f) -> f (a -> b) -> f a -> f b
    f <*> x = liftA2 id f x

## Monad

The `Monad` class contains one function, `(>>=)` (pronounced "bind"), on top of what is already present in an `Applicative` (such as `(<*>)` and `pure`).

    typeclass (Applicative m) => Monad m where
        (>>=) : m a -> (a -> m b) -> m b

This may not seem like a huge innovation at a glance, but in fact monad bind is the main thing that allows us to do useful work in functional languages, which is why we refer primarily to the `IO` *monad* rather than the `IO` functor or applicative. To see why, consider an example:

    askForName : IO String
    askForName = -- ask the user for their name somehow

    printGreeting : String -> IO ()
    printGreeting name = println ("Hello, " ++ name ++ "!")

Try as we might, there is no way, using only `Functor` or `Applicative` operations, to ask the user for their name and then *chain that result* into `printGreeting`. You may have already deduced how to do this in the `IO` monad:

    askForNameAndGreet :: IO ()
    askForNameAndGreet = askForName >>= printGreeting

This is why monads are so exceptionally important for making nontrivial functional programs: they let us pipe the results of side-effectful operations into each other. This is basically what an interactive program is all about! Of course, the type of side effects `IO a` is not the only monad, and for other types, the semantics of "what it means for this thing to be a monad" vary. We will be going over many of these in [Monads: A Field Guide](./monad-field-guide.html).

## Traversable

`Traversable` is used for data structures that can contain multiple actions, which can be performed in a specific order. For instance, given a list of `IO` actions which ask the user to type a word on the console:

    askForWordsThreeTimes : [IO String]
    askForWordsThreeTimes = [askForWord, askForWord, askForWord]

    askForWord : IO String
    -- something

The type of `askForWordsThreeTimes` is somewhat inconvenient, though. We really just want *one* action, which contains the results. For this type of situation, we use `traverse`:

    typeclass (Functor t, Foldable t) => Traversable t where
        traverse : (Applicative f) => (a -> f b) -> t a -> f (t b)

In the type signature for `traverse`, think of `a` as an "input type" and `b` as an "output type". `t a` is a collection of inputs in some structure `t`, and the result is one big action that contains the results in the same structure: `f (t b)`.

There is a helper function `sequence` that can be used if your structure already has a bunch of actions, and you just need to collect them instead of mapping:

    sequence : (Traversable t, Applicative f) : t (f a) -> f (t a)

Using this, we can get

    askForThreeWords : IO [String]
    askForThreeWords = sequence askForWordsThreeTimes

Note that we're not combining the results in any way; we're just turning, for instance, a list of actions into one action with a list of results. The order in which those actions is performed is always the same for any given structure, which is defined by the `traverse` implementation for that structure. For instance, lists are always evaluated from left to right:

    -- this was mostly borrowed from Haskell's implementation
    instance Traversable []
        traverse f = foldr cons_f (pure []) where
            cons_f x ys = liftA2 (::) (f x) ys

`sequence` can also be used for some funny edge cases, like if you accidentally end up with a `Maybe (IO a)` when you really want an `IO (Maybe a)`.
