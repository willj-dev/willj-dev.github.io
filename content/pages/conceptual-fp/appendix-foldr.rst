------------------------
title: 'Appendix: Right Folds From The Left'
------------------------

Here we are going to look into how to implement a right fold generically, given only a left fold and no other information about the data structure. The idea is that we fold the structure up, from the left, *into a function*, where the resulting function is designed to evaluate the right-most values first. Here’s what that looks like:

.. code:: pseudoml

   foldr : Foldable t => (a -> b -> b) -> b -> t a -> b
   foldr f z t = (foldl foo bar t) z where
       foo = _
       bar = _

This is pretty much a direct translation of the idea above: we left-fold (somehow) the structure :pseudoml:`t` into a function that evaluates from the right, then kick it off with the given starting value. Now we just need to figure out what to use for :pseudoml:`foo` and :pseudoml:`bar`! Let’s start by looking at their types, to see if they give us any clues. We know that :pseudoml:`foldl foo bar t` must be a function, and it should have type :pseudoml:`b -> b`. If we compare that to the type of :pseudoml:`foldl`,

.. code:: pseudoml

   foldl : Foldable t => (c -> a -> c) -> c -> t a -> c

in order to have the correct result type, we must have

.. code:: pseudoml

   foo : (b -> b) -> a -> (b -> b)
   bar : (b -> b)

The second one is easy: whenever you need a value that fits that type signature, it almost certainly should be the identity function :pseudoml:`id`. What about :pseudoml:`foo`? Well let’s treat it as a function of two arguments:

.. code:: pseudoml

   foo : (b -> b) -> a -> (b -> b)
   foo g x = _

At this point, let’s consider what gadgets we have available to us. We haven’t yet used :pseudoml:`f : a -> b -> b`, and now we also have :pseudoml:`g : b -> b` and :pseudoml:`x : a`. In general, unless you have a good reason, you want to try to use all of the variables you have handy. Interestingly, :pseudoml:`f x` will have type :pseudoml:`b -> b`, so what if we just compose that with :pseudoml:`g`?

.. code:: pseudoml

   foldr : Foldable t => (a -> b -> b) -> b -> t a -> b
       foldr f z t = (foldl foo id t) z where
           foo g x = g . f x

If you try this out, you’ll find that this definition works exactly as we wanted it to! This is actually somewhat amazing, which is a pretty common occurrence with "type-driven development" as this method is usually called. We could have arrived at the same result if we sat down and worked out exactly what it means to "left-fold a structure into a function that executes a right-fold", but that would have required a lot more noodling.

To be honest, though, we cheated a little bit. Doing the composition in :pseudoml:`foo` the other way around would have typechecked, but produces the wrong results:

.. code:: pseudoml

   notFoldr : Foldable t => (a -> b -> b) -> b -> t a -> b
       notFoldr f z t = (foldl foo id t) z where
           foo g x = f x . g

If you work out an example, this turns out to look like a right fold…but for a reversed input! This is the price of type-driven development: sometimes there is more than one choice to fill in a value for a given type, and the only way to determine which choice is correct is by testing it out yourself. Static type checking is not a substitute for *all* tests!
