------------------------
title: 'Appendix: Language-Specific Caveats'
------------------------

Here we will discuss some "gotchas" or signficant syntactic deviations from some popular programming languages that might catch the inexperienced reader off-guard if they try to apply the concepts above to that language. Once again, the goal is not to teach a specific language; this is just a collection of warnings and disclaimers that would otherwise have cluttered the main narrative.

## Scala / `cats`{.plain}

-  typeclasses via implicits/traits

## Haskell

- type name translations (Maybe vs Optional, etc)
- data vs newtype
- newtype for re-implementing typeclasses
- dangers with laziness (e.g. foldr vs foldl)
- reader, writer, state defined in terms of their transformers

## TypeScript / `fp-ts`{.plain}

- syntax difficulties
- type name translations (Option vs Optional, etc)

## Idris

- labelled interfaces
