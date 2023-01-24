------------------------
title: Conceptual Guide to Functional Programming
blurb: >
  A collection of pages amounting to a smallish book which hopes to provide intuitive motivation for functional programming concepts which might otherwise mystify interested learners. This is done in terms of a "pseudo-ML" toy language which is definitely not Haskell.
------------------------

The primary goal of this document is to provide a conceptual overview and motiviation for functional programming concepts in a more or less language-agnostic way.

Writing a conceptual guide, rather than aiming to teach a particular language, is somewhat quirky. A cynical but not completely inaccurate reader might observe that this choice (a) absolves us from having to attend to messy practical matters (build systems, disagreeable syntax....), and (b) is a marketing decision because no one will pick up "Yet Another Haskell Book: This Time It'll Make Sense, We Promise!" However, we believe this choice is in the reader's interest too; to someone who is completely new to functional programming from even a conceptual standpoint, gaining intuition about how programs are organized and composed is difficult to do simultaneously with learning the practical parts like performance concerns and library management.

Ultimately this is still going to be about programming, so there will be "code" snippets. However, it is important to emphasize again that it is **not** a goal to teach any one programming language. Some examples will be written in languages such as Java or ECMAScript for comparison purposes, since they should be recognizable to most readers. That being said, [Language-Specific Caveats][] provides a brief glance over some of the ways in which particular languages deviate from the names or styles introduced below.

For functional-style snippets, we will be using a syntax inspired by ML-family languages like Haskell because those languages were designed from the ground up to make programming in a functional style as efficient as possible. It is called "PseudoML" because it is sufficient to illustrate concepts, but it lacks many of the features that would make it suitable for parsing by a compiler; it is intended only for human readers. [PseudoML Syntax][] goes over that in more detail, but first we should introduce functional programming in general.
