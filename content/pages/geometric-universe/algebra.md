------------------------
title: Geometric Algebra
------------------------

## Fundamentals

### What's in an Algebra?

An algebra is made of *elements*, which we normally call by single-letter names like $a$, $b$, $x$, or $y$; and *operations*, like addition and multiplication, along with various rules to describe how these pieces interact.

We can start somewhere familiar: the real numbers, like ${}2$ or $\pi$. Within GA we will refer to these as *scalars*; this name comes from the fact that multiplying any element by a scalar "scales it" by that factor. Thus, ${}3a$ is three times "bigger" (in a somewhat loose way) than $a$.

There are also *vectors*, which will be doing most of the legwork for us. 

### Addition

We can add elements together; addition is *associative*, so it doesn't matter how a series of additions are grouped:

$$ a + b + c = (a + b) + c = a + (b + c) $$

and *commutative*, so it doesn't matter which order the elements are written in:

$$ a + b = b + a $$

Incidentally, addition is still allowed even between mixtures of scalars, vectors, and higher-order elements. Thus, a given element may have a *scalar part*, a *vector part*, and so forth. High school vector algebra forbids constructions like ${}3 + 2\vec x$, but from the perspective of GA, this restriction is unnecessary and actually prevents us from accessing some of the neater features we explore below. In fact, if you have worked with complex numbers before, this is exactly the same way that $z = a + ib$ has a *real part* ($a$) and an *imaginary part* ($b$), and below we will be able to construct an element that squares to $-1$ without having to artificially include the "imaginary unit" $i$ by fiat!

### Multiplication

Multiplication in GA is going to be where we will start departing more significantly from high school algebra. If you studied physics or vector calculus, you may already be familiar with the existence of multiple "kinds" of multiplication, as in the dot product $\vec a \cdot \vec b$ and the cross product $\vec a \times \vec b$. In GA, we can interpret these as two sides of one overall *geometric product*, which we simply write like $ab$.

In order to do so, however, we will need to allow the geometric product to be *non-commutative*:

$$ ab \neq ba,\ \text{in general.} $$

Of course, we don't mean to imply that it's *never* true---we just need to be careful not to take it for granted. For instance, obviously scalar multiplication still commutes, so that e.g. ${}3\times 2 = 2 \times 3 = 6$. But unless we know that a given element is a scalar, or can commute with adjacent elements for some other reason, we can't just rearrange products without a care. This may seem like a strange feature to desire in our algebra, but as we shall see, the amount of mileage we can get out of letting go of commutativity in our products is pretty astounding!

The geometric product $ab$ is made of two parts: the *inner product* $a \cdot b$ and the *outer product* $a \wedge b$:

$$ ab = a \cdot b + a \wedge b. $$

Like the dot product, the inner product commutes:

$$ a \cdot b = b \cdot a. $$

And like the cross product, the outer product *anticommutes*:

$$ a \wedge b = - b \wedge a. $$

Fiddling around with these three equations yields

$$ a \cdot b = \frac{1}{2} (ab + ba) $$

and

$$ a \wedge b = \frac{1}{2} (ab - ba). $$

To be precise, we will treat these last two as the *definitions* of the inner and outer products, respectively, and the relation $ab = a \cdot b + a \wedge b$ as a consequence of these definitions, rather than the other way around.

### Distributive Law

As far as algebraic rules go, one of the mainstays is the good old-fashioned distributive law, which defines one of the main ways that multiplication and addition interact.

$$ a(b + c) = ab + ac $$

Unlike its relative the commutative law, we are going to keep this one set in stone; it's true always.

## Scalar Algebra; Vector Algebra; Complex Numbers and Beyond

- Using only scalars gives us high school algebra (the algebra of a field over real numbers)
- Using only vectors gives us linear algebra, and in three dimensions we can recover dot/cross products
- Pseudoscalar as the "imaginary unit"; multivectors as an extension of encoding complex numbers as a + bi

## Symmetry and Invariance

- Definitions
- Symmetry Groups
- Conformal Symmetry
- Conformal Geometry of Spacetime

## Geometric Calculus

- Limit; derivative; integral
- Stokes; FTC and other special cases

## Calculus of Variations and the Euler-Lagrange Equations

"Solutions" in GP are specifically solutions to the Euler-Lagrange equations, which are generally coupled partial differential equations and, in practice, almost always impossible to solve directly. This makes the GUH, in one sense, disappointing: what's the point of using math to describe physical laws if all we get out of it is equations we can't solve? But really it's not as bad as all that. In fact, there are many ways to information out of these equations without actually solving them; this is essentially what theoretical physics is, and recently we have also been able to tackle them numerically using computer algorithms. Besides, the simple cases that we *can* solve exactly are still valuable for human understanding. We are in a sense doing the same thing humans usually do when faced with a complicated math problem: break it down into easily digestible chunks, and use mathematical notation to keep track of all the little bits so that we don't get overwhelmed.
