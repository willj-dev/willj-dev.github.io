
.. default-role:: pseudoml

.. PANDOC BUG: the span's class is always set to the roleName, regardless
    of what is actually specified in the :class: field body.
    Fixed in https://github.com/jgm/pandoc/pull/7700 (not yet released)
.. role:: caption
  :class: asdf-see-comment-above

.. role:: pseudoml(code)
  :language: pseudoml

$body$

.. _PseudoML Syntax: ./basic-concepts.html#pseudoml-syntax

.. _Language-Specific Caveats: ./appendix-lang.html

.. _Right Folds from the Left: ./appendix-foldr.html
