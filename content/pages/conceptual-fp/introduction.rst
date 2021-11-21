------------------------
title: Introduction to Functional Programming
------------------------

Functional Programming: What And Why
============================================

Let's begin at the beginning. Programming languages are an abstraction over instructions that can be run by your CPU. These instructions are fundamentally *imperative*: we are telling the machine to read and write data between memory registers. Low-level languages like C are designed to be very thin, platform-independent wrappers over these instructions, with minimal control structures (for loops, structs) to simplify the work of reading and writing code.

C also allowed giving a name to a repeatable set of instructions that can be called from other parts of the program. These are usually called functions, but *routine* might be a better word. Object-oriented languages like C++ or Java provide an additional level of abstraction over C, but keep the same imperative flavor: a *class* describes a set of data and routines to operate on that data.

By contrast, a (purely) functional programming language departs from the imperative style entirely. They are *declarative*: we describe *what things are* rather than *how to do them*, and the compiler takes care of translating what we write into imperative language. To illustrate the difference, below we show some example code, written in (a) imperative-style ECMAScript and (b) declarative pseudo-ML. 

.. code:: javascript

  function makeGreeting(name, todos) {
      const firstPart = `Hi, ${name}!`;
      let secondPart;
      if (todos && todos.length > 0) {
          let todoMsg = todos[0];
          for (let i = 1; i < todos.length; i++) {
              todoMsg += `; ${todos[i]}`;
          }
          secondPart = `Here are your tasks for the day: ${todoMsg}`;
      } else {
          secondPart = 'Congrats, you're all done for the day!';
      }
      return `${firstPart} ${secondPart}`
  }

:caption:`(a) An example of imperative style in ECMAScript.`

.. code:: pseudoml

  makeGreeting name todos =
      sayHello name ++ " " ++ todosMessage todos 

  sayHello name = "Hi, " ++ name ++ "!"

  todosMessage []        = "Congrats, you're all done for the day!"
  todosMessage (t :: ts) = todosIntro ++ todosList where
      todosIntro = "Here are your tasks for the day: " 
      todosList  = t ++ listRemainingTodos ts

  listRemainingTodos []        = "."
  listRemainingTodos (t :: ts) = "; " ++ t ++ listRemainingTodos ts

:caption:`(b) An example of declarative style in PseudoML.`

Both examples illustrate breaking the problem down into smaller pieces, but that's where the similarities end. A loop, for instance, is an imperative construct: "repeatedly execute this block of instructions". In functional programming, iterating over a list is generally accomplished by doing something with the head of the list and then recursing through the remainder (as in :pseudoml:`listRemainingTodos`). Pattern matching on function arguments takes the place of if blocks to direct execution. These and other quirks of functional programming are all due to the fundamental difference between imperative and declarative styles.

That takes care of the "what". Why do people like functional programming? What does it gain over other paradigms? As with all things, it comes down to a combination of personal preference and math.

Personal Preference
''''''''''''''''''''''''''''''''''''''

People who prefer functional programming tend to be the same set of people who like statically-typed languages with very smart compilers. This is where basically all of the room for different opinions resides; I'll start by going over some of the reasoning behind that preference (as well as some of the arguments against it). Later on, I'll go over how that has to do with functional programming.

First, some definitions. A *type system* is a way of assigning a property called *type* to a given *value* in the programming language. Types are a human construct that tell us something about what a program means: after all, inside the silicon we're just shuffling bits around. But when we have a function called :pseudoml:`stringLength` and let the computer plug any random bits into it, there's a good chance that whatever comes out won't even make sense as an integer, let alone a measure of length!

To prevent this kind of faux pas, we tell the compiler (or the compiler infers from usage, if it's a smarty pants) that this function should only be called with one argument, a string, and the return value should only be used like an integer. We are in effect getting some documentation for free: clients can read the type signature of a function and understand quite a lot about its behavior, especially if we can guarantee that there are no "side effects" (more on that later). But that's not all! Static type checking can be considered a form of testing: not only does the compiler guarantee that the function is used correctly, but it can also guarantee that the *definition* of the function upholds the type signature we claimed it should!

Strong static typing isn't a universal solution, though. We are in effect front-loading the work of discovering funny edge cases to compile-time, rather than run-time. Dynamically-typed, interpreted languages like Python and ECMAScript derive a lot of their appeal from the fact that they make it easy to crack out a lot of code *fast*, and as long as it's syntactically correct it will run. With adequate tests, you can be reasonably sure that the code is correct; for small projects or scripts, this is frequently much more efficient.

But what happens when your project gets popular and starts being used by other people? Even with excellent documentation (which, of course, is always available) there's nothing preventing someone from accidentally passing a string into a function that expects a number. Can you guarantee that your code will fail quickly in such an event, without putting the client's system into a corrupted state? *Should* you be expected to verify that at runtime?

Statically-typechecked code *drastically* reduces the surface area for runtime errors, filtering out the noise from programmer errors or typos, and pulling them all the way to the front of the development cycle. For situations where that is desirable, like critical enterprise software that needs firm guarantees of correctness, the cost of the additional developer time fighting with the compiler is well worth avoiding potentially costly runtime issues. Even in less critical software, the ability of IDEs to typecheck code as you write it reduces the feedback loop even further, with the strong guarantee of correctness a happy side benefit.

Long story short, it comes down to: would you rather write possibly correct code really quickly, or really correct code possibly quickly? For various reasons that we'll explore presently, functional programming is an excellent choice for anyone who picks the latter option.

Math
''''''''''''''''''''''''''''''''''''''

Functional programming languages are designed to be very close to the language that mathematicians use to prove things like "does this algorithm terminate?" This makes it possible to write an *exceptionally* smart compiler. Some of the questions that mathematicians might ask are

- Can it be guaranteed that this program doesn't have an infinite loop?
- Can it be guaranteed that this program will run without an error?
- Can it be guaranteed that this program won't set my grandma on fire?


The language that mathematicians and logicians use to describe and (attempt to) answer these questions is called the *lambda calculus*, which is very much out of the scope of this paper. Atop that framework is a language of types, which lets us say things like "Here is a function called :pseudoml:`stringLength`; if you plug in a string, this will return an integer representing the number of characters in that string; no other inputs are allowed." At this point, a compiler can check things like

- Reject any program that attempts to plug something other than a string into :pseudoml:`stringLength`
- Reject any program that attempts to use the output of :pseudoml:`stringLength` as anything other than an integer
- Reject the program if :pseudoml:`stringLength` returns something other than an integer
- Reject the program if :pseudoml:`stringLength` does not accept any valid string

and (importantly) it is possible to *mathematically prove* that the compiler answers those questions correctly. It is the ultimate in test technology: rather than relying on a mere finite number of example cases as in traditional testing, we can rely on **Mathematical Truth (TM)**!

Now, all that being said, software engineers shouldn't be expected to have math degrees! None of that background is required to actually *use* the FP toolkit, in the same way that we don't need to know the instruction set for the processors in our laptops. It is just a convenience that we can take for granted when we write our code and it compiles.

Functional Programming: Why Not?
============================================

The mathematical heritage of functional programming has given it a reputation for being difficult to understand, or just a research toy for mathematicians and academics. There is a reason why this image developed, but it is not really well-deserved.

Consider the languages most of us use today. From their earliest ancestors, they were developed by computer nerds, who just wanted to tinker around and play tetris and talk to others of their kind on message boards.

By contrast, as we saw above, many of the contributions that formed the foundation for functional programming languages came from mathematicians and logicians. This is actually a very useful thing for us, but it comes with a price: they were there first, so they got to pick the names. Mathematicians are perfectly happy floating around in wizard robes and unironically saying things like "oh yes Veronica, monads are just monoids in the category of endofunctors!" Engineers live much closer to the real world and do not have time for such frippery, and have satisfied themselves with more normal-sounding terms like "class", "object", "singleton", or "factory".

So, yes, there will be some unfamiliar and mystical-sounding terms ahead. But fear not: they are just names, and the things they represent have solid programmer-friendly meanings.
