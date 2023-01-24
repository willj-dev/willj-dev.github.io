------------------------
title: Basic Concepts
------------------------

PseudoML Syntax
-----------------

Type declarations and assignments look like this:

.. code:: pseudoml

   myInt : Int
   myInt = 5

   myString : String
   myString = "Hello, world!"

   type String = [Char]

   myCharList : String
   myCharList = ['H', 'e', 'l', 'l', 'o']

The `type` keyword creates a *type synonym*; this is just a different label, sometimes to save on keyboard work for a frequently-used but complex type name, or sometimes as a form of documentation (as in `type UserId = String`). Brand new types are declared with `data`\:

.. code:: pseudoml

   data Optional a = Just a | None

   justFive : Optional Int
   justFive = Just 5

   noInt : Optional Int
   noInt = None

Functions are defined like so:

.. code:: pseudoml

   stringLength : String -> Int
   stringLength ""     = 0
   stringLength (_::s) = 1 + stringLength s

Note the *pattern matching* on the left-hand side of the `=`. This should be read as "`stringLength` called with an empty string returns 0; otherwise, we know it isn’t empty, so discard the head of the list and add 1 to the length of the remainder."

Function application (that is, "calling" a function) is achieved by simply listing the arguments after the function name, as in `f x y z` which calls a function `f` with three arguments. Arguments are always applied from left to right:

.. code:: pseudoml

   myValue = f x y z -- same as: (((f x) y ) z)

and in general, it has higher precedence than any other operator:

.. code:: pseudoml

   myOtherValue = f x + g y -- same as: (f x) + (g y)

Composition & Currying
----------------------

Suppose we have

.. code:: pseudoml

   stringLength : String -> Int
   isOdd : Int -> Bool

We could write

.. code:: pseudoml

   stringHasOddLength : String -> Bool
   stringHasOddLength s = isOdd (stringLength s)

but that gets tedious quickly if we are chaining several functions together. The idiomatic way to do this is to use *function composition*:

.. code:: pseudoml

   stringHasOddLength = isOdd . stringLength

The `.` in that definition is itself a function:

.. code:: pseudoml

   (.) : (b -> c) -> (a -> b) -> a -> c
   (f . g) x = f (g x)

It says "take the output of the function on the right, and plug it into the function on the left". Note that the new definition of `stringHasOddLength` does not actually bind a name for the argument! We *can* do that, as in

.. code:: pseudoml

   stringHasOddLength x = (isOdd . stringLength) x

but there is no need. We’re just saying "`stringHasOddLength` is the result of composing these two functions." This is called, somewhat misleadingly, "point-free style." [1]_ You can do it in ES too:

.. code:: js

   const compose = (g, f) => x => g(f(x));
   const stringLength = s => s.length;
   const isOdd = i => i % 2 === 1;
   const stringHasOddLength = compose(isOdd, stringLength);

If you were to take a static type analysis tool to this code, it would hopefully resolve the type of `stringHasOddLength` as a function from strings to booleans, despite not having actually used an explicit function definition that binds an argument name.

Point-free style is related to the concept of "currying", which is named after a person named Haskell Curry, not the food. If a function takes two arguments, and you feed it only one, the result is a function that takes one argument:

.. code:: pseudoml

   stringLengths = map stringLength

The `map` function is the usual: it takes a function `(a -> b)` and a list `[a]`, and then returns the result of applying the given function to each element of the list. So if we stare at this definition, since `stringLength : String -> Int`, we can deduce that `stringLengths : [String] -> [Int]`.

.. [1]
   Mathematicians again. "Point" means "function argument"; "point-free" means "defined without binding a name to the arguments".

Recursion
---------

Recursion is much more important in functional programming than in imperative programming, because recursion is the primary way to implement loops (in addition to the various other uses that it has in common with non-functional code). We will have much more to say about recursion later on (see `Recursion Schemes`_), but for now we can go over some basic examples to get us started.

Here’s how we might implement `map` over lists:

.. code:: pseudoml

   map : (a -> b) -> [a] -> [b]
   map _ []      = []
   map f (x::xs) = f x :: map f xs

The `::` constructor sticks an element on the head of a list. The first equation takes care of the base case (stop recursing once we hit the end of the list), and the second one says to apply the function to the head of the list, and then do the same thing on to the remainder.

Here’s another example, which works rather like Python’s :python:`range()` with one argument:

.. code:: pseudoml

   range : Int -> [Int]
   range x = if x < 0 then [] else (x - 1) :: range (x - 1)

Purity
------

There are a couple of meanings of *pure*, depending on context:

Pure Functional Languages
~~~~~~~~~~~~~~~~~~~~~~~~~

A *pure functional language*, such as Haskell, is a language that only supports functional-style programming, with no way to represent other programming patterns like object-oriented code. These are usually contrasted with "functional-first" languages like F# or, depending on who you are talking to, Scala; in these languages, functional and object-oriented styles can coexist.

PseudoML is a pure functional language, because it was invented [2]_ to illustrate functional programming concepts, so attempting to support additional syntax would just be distracting.

.. [2]
   Well, perhaps it is more accurate to say "shamelessly cobbled together from bits of existing languages"

Pure Functions
~~~~~~~~~~~~~~

A *pure function* is a function that does not have any "side effects", such as updating a global state, writing to (or reading from!) a file, and so on. You don’t need a pure functional language to write a pure function; here’s one in ES:

.. code:: js

   const pureAdd = (x, y) => x + y;

Pure functions are important for a couple of reasons. From a practical standpoint, they are easy to test; without any global state that can be corrupted by another process, or flaky I/O operations, we know that a pure function called with the same arguments will always produce the same result. From the standpoint of implementing a language, we can get a form of memoization for free: results of pure functions can be cached since the compiler can guarantee that there’s no way for the result to change from one call to the next.

Of course, it doesn’t make sense for a language to only support pure functions; the *whole point* of running a program is to get the side effects! Side-effectful operations belong to their own type, `IO a`. For instance, an `IO Int` represents an operation that does something unspecified and then returns an `Int`. They are, therefore, somewhat spooky; running an `IO` "action" is dangerous if you don’t know where it’s been, since even an innocuous-seeming `IO Int` could represent the action "wipe the hard drive and then return the number of dirty pictures that this program emailed your grandma".

To facilitate effective testing (and out of a desire to be tidy), functional programmers generally try to keep as much of their code pure as possible. Consider, for instance, parsing an image file and returning the number of red pixels present in the image. Side effects are only required when reading the file and then printing the result to the console; everything in between is pure operations on the contents of the file, which is just a series of bytes. Folks who are new to functional programming often find the restriction of side effects to `IO` grating, but that is just after a lifetime of being able to sprinkle side effects around anywhere. Once you get used to structuring your code appropriately, it becomes second nature, and eventually the cavalier attitude of other languages toward side effects starts to feel a bit rude!

.. code:: js

   function justAddIPromise(x, y) {
       window.open('http://downloadvirus.biz');
       sendDirtyEmails('grandma@oldfolks.net');
       console.log('ha ha you suck');
       return x + y;
   }

Strictness & Laziness
---------------------

Strictness refers to whether or not a given expression in the source code is actually evaluated by the processor. One famous example of "non-strict semantics" is what we call short-circuiting in boolean operators:

.. code:: js

   const everythingIsFine = true || fireAllMissiles();

The expression on the left-hand side of the `||` is always evaluated, but the right-hand side may not be, if the processor knows by then what the overall expression will evaluate to. On the other hand, most other operations, such as function calls, follow "strict semantics":

.. code:: js

   function uhoh(stuff, things) {
       console.log(`here is some stuff: ${stuff}`);
   }

   uhoh('my stuff', fireAllMissiles());

Whenever you call a function, the arguments are always evaluated *first*, and then they are passed to the body of the function—regardless of whether the function body even refers to every argument it’s given.

A term that is frequently used alongside "non-strict" is "lazy". Laziness is a way to *implement* non-strictness. In a lazy language, all expressions are implicitly replaced by zero-argument functions that *return* the expression’s value, called a "thunk":

.. code:: js

   const two = 2;
   const twoThunk = () => 2;

This is done behind the scenes, or else the code would be unacceptably cluttered. Although it makes it somewhat hard to decide whether some code will execute before or after another, the only times where that usually matters (namely, executing side-effectful actions) are wrapped up in the `IO` monad (more on that later!) which has a sense of "do this before that" built-in to the structure.

PseudoML as used here will in general be non-strict, though in the few places where it matters we will point that out. Specific languages have different ways to achieve strictness/non-strictness when that is not the default behavior, so we will leave it up to the reader to determine how to implement that in the wild.

Typeclasses
-----------

A typeclass is a set of functions that can be overloaded to work with any type. Defining how those functions work on a particular type is called *implementing* that typeclass. The most basic typeclasses are `Eq`, `Ord`, and `Show`, which we will go over here. In the next section, we’ll start getting into some of the meatier examples.

Languages with this concept usually include several functions in the typeclass definition, many of which may be given a "default" definition in terms of some minimal set that must be implemented. This is entirely for practical purposes; in specific cases, there may be a more efficient way to implement one of the "extra" functions. Such considerations are an implementation detail outside our scope, so we will limit our typeclasses to the minimal set of functions, and define the other ones separately when they are needed.

Eq
~~~~~~~~~~~~~~~~

A data type can be made an instance of `Eq` if its values can be compared as equal or not equal. It is defined like this:

.. code:: pseudoml

   typeclass Eq a
       (==) : a -> a -> Bool

This says "`a` is an instance of `Eq` if there is an implementation for the `(==)` function." As an example, consider a data type representing the three primary colors:

.. code:: pseudoml

   data PrimaryColor = Red | Blue | Green

   instance Eq PrimaryColor
       Red   == Red   = True
       Blue  == Blue  = True
       Green == Green = True
       _     == _     = False

For "obvious" cases like this, the compiler can frequently implement this sort of thing on its own, but that functionality is generally language-dependent.

Polymorphic functions are written like this:

.. code:: pseudoml

   elem : Eq a => a -> [a] -> Bool
   elem _ []      = False
   elem e (x::xs) = e == x || elem e xs

The `=>` notation says that `a` can be any type, as long as it has an `Eq` instance. This is a function of two arguments: something to look for in a list, and the list in which to look. The second line says "nothing is in an empty list." The third line says "check the first element in the list; if it is equal to what you’re looking for, return `True`; otherwise, keep looking in the rest of the list."

Incidentally, how do we compare lists? Two lists are equal if they have the same elements in the same order. This means that we need a way to compare the elements to see if they’re equal too. So we might write `Eq [a]` like:

.. code:: pseudoml

   instance Eq a => Eq [a]
       []      == []      = True
       (x::xs) == (y::ys) = x == y && xs == ys
       _       == _       = False

This says "two empty lists are equal; two nonempty lists are equal if their heads and tails are equal; otherwise, they are never equal." Note that the `Eq a` constraint is what lets us use `x == y`.

Ord
~~~~~~~~~~~~~~~~~~~~~~~~~~~

`Ord` types support a notion of "ordering". The class is defined like so:

.. code:: pseudoml

   data Ordering = LT | EQ | GT

   typeclass Eq a => Ord a
       compare : a -> a -> Ordering

Similarly to its use in type signatures, the `=>` at the top says that in order to be an `Ord`, the type must also implement `Eq`. The usual operators like `<` are then defined in terms of `compare`; for instance,

.. code:: pseudoml

   x <  y = compare x y == LT
   x >= y = not (x < y)
   max x y = if x >= y then x else y

One of the nifty examples using `Ord` is a recursive implementation of the QuickSort algorithm.

.. code:: pseudoml

   sort : (Ord a) => [a] => [a]
   sort []      = []
   sort (x::xs) = sort left ++ (x :: sort right) where
       left  = filter (<  x) xs
       right = filter (>= x) xs

This is a pretty popular canonical "look at how much cleaner FP is!" example, but it emphasizes cuteness over performance. If you are tempted to compare it favorably against an in-place sort implemented in e.g. C, keep in mind that the in-place algorithm is going to be much more space-efficient, and almost certainly less obvious than this little toy algorithm.

Show
~~~~~~~~~~~~~~~~~~~~

`Show` is for types that can be represented as a string:

.. code:: pseudoml

   typeclass Show a
       show : a -> String

Most languages also have the ability to generate instances of `Show` (or equivalent) for you. This is also simplified from the definition you might see in the wild, which is designed to support efficiently building the output string for nested structures. Just for fun, here’s an example instance for lists whose elements are themselves `Show`\ able.

.. code:: pseudoml

   instance Show a => Show [a]
       show []      = "[]"
       show (x::xs) = "[" ++ show x ++ showNextElems xs ++ "]"

   showNextElems : Show a => [a] -> String
   showNextElems []      = ""
   showNextElems (x::xs) = ", " ++ show x ++ showNextElems xs
