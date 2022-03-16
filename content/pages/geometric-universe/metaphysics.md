------------------------
title: Metaphysics
------------------------

## Reality, the Mind-Body Problem, and Science

We should start with the *really* fundamental stuff so that we can properly interpret the statement that the "universe" is "geometric". Following Descartes' example, we'll start with the *mind*, and, specifically, *me* ^[Not to play favorites or anything; it's just that if we're talking about whether people exist, it makes sense to start with the person making that claim!]:

The Doctrine of Existence
:   I exist, and perceive the world around me as well as my own existence. This thing-that-perceives-itself is my *mind*, and it is apparently attached to a particular human body on a particular planet in a particular solar system, and so on.

The Doctrine Against Solipsism
:   There are other minds that exist independently of my own.

The Doctrine of Objective Reality
:   There is an *objective reality*, which exists regardless of whether or not any minds are around to perceive it. Minds experience objective reality from independent perspectives, but it is possible for individuals to compare observations and distinguish three types of statements: *true statements* which are observer-independent; *false statements* for which a counterexample can be found; and *subjective statements*, which are everything else.

We must be careful to distinguish between a *hypothesis*, which is a statement that can be judged as true or false without needing to refer to an individual perspective, and a *doctrine*, which can't. For instance, it's easy for me to demonstrate my own existence to myself (yep, I'm still here!), but I don't know of any way for someone else to confirm my observation.

These three doctrines are not required for the universe to work, but they are nevertheless important because they allow us to sensibly define *science*: the systematic study of patterns in objective reality. This doesn't mean anything if we don't presuppose that multiple individuals exist and can make independent observations of an objective reality.

The science of biology has made it clear the mind is affected by the body (primarily via the brain). What isn't clear is whether or not it goes the other way: does my mind actually control my body, or is that just an illusion? This is another famously unanswered (maybe even unanswerable?) question, and even though its answer isn't required to be able to do science, free will is an important and helpful assumption once individuals start trying to form societies, which is an important prerequisite for science. The *mind-body problem* refers to the fact that we really have no idea why minds are attached to specific bodies (instead of, say, having one "hive mind" that simultaneously experiences the perspectives of separate physical bodies).

The Doctrine of Free Will
:   The mind-body connection is a two-way street: the mind can affect the body, in addition to the other way around.

This allows us to define "personal agency" (people can choose to do things) and "justice" (people are responsible for the consequences of things they choose to do, and reasonable punishment is justified for excessively harmful behavior). [A Millennial Utopia](/millennial-utopia) explores the possibility of forming a social contract around these ideas.

From now on, we will just say "reality" with the implication that we are only making objective statements. We should also note that even though our focus here is on the objective part of reality, we do not intend to imply that subjective reality is any less *real*. It is simply out of the scope of this narrative; this includes the ancient human tradition of believing in the truth or falsehood of unfalsifiable statements. We will simply acknowledge that objective truth, by definition, does not care one way or the other; the reader is encouraged to find their own way to understand and relate objective facts to their own subjective experience.

## Patterns, Information, and Algebra

If science in general is the study of patterns in objective reality, mathematics is the particular branch of science that studies the structure and relationships between those patterns. This is the sense in which math is more *abstract* than, say, sociology. This doesn't make it any less objective; the existence and identity of patterns---which sometimes gets the portentous name of *mathematical truth*---is part of objective reality. This follows from the Doctrine Against Solipsism and the fact that we can recognize patterns at all (so they exist); the fact that people can invent independent instances of the same pattern proves its objectivity. We also have a way to distinguish between instances of patterns in the physical world (*information*) and the abstract concepts underlying them; this is the difference between a circle on a whiteboard and the *idea* of a circle, which has a certain quality of "idealism" that does not seem to occur in physical things.

Humans have a limited amount of "processing space", so we invented *algebra*: a set of rules for representing and manipulating mathematical concepts in writing. The particular set of fundamental rules we choose (*axioms*) affects the kinds of things it is possible to write or prove; in fact, we generally just say that the set of axioms *defines an algebra*. The algebra that most people learn in grade school has its own scary technical name, and there is a whole ecosystem of important algebras; we will be focusing on one in particular, the *geometric algebra*.

We should also briefly mention an important and frequently misunderstood feature of mathematics: G\"odel's *incompleteness theorems*. It amounts to saying that no matter what axioms we start with for our mathematical language, it is always possible to find a statement that cannot be proven or disproven from those axioms. This sounds somewhat dire, but we can give it a more palatable interpretation: "We can write down only those things that we know how to write down." The incompleteness theorems specifically refer to the incompleteness of the *written rules*, and really we should not be particularly surprised by this result; humans have to invent new words for things all the time, so it makes sense to accept that we can always discover new mathematical structures that can't be expressed using our existing rules!

## Scale Dependence of Observation

Before we started looking at really small stuff (which gave us quantum mechanics) or really big stuff (which gave us general relativity), we only ever observed the universe at the *classical scale*: the realm of phenomena from atoms (as long as we only think about them as uncomplicated specks) up to the solar system (as long as you don't look too hard at things like [the orbit of Mercury](https://en.wikipedia.org/wiki/Tests_of_general_relativity#Perihelion_precession_of_Mercury)). But once we started poking around outside those limits, things started getting (subjectively, at least) *weird*. The math still works, but it no longer describes things in a way that humans can intuitively grasp. Objectively, of course, this need not concern us; by now several generations of physicists have been able to do their jobs by becoming very good at math, assuming that the algebra will work out, and painstakingly chasing symbols around whiteboards until they find something that can be compared to experimental data. The reason why we bother is that, astoundingly, *this works!* The Standard Model of particle physics is famously one of the most well-tested theories in the history of science, but the actual weight of that fact doesn't really become clear until you start wading into the "deep end" where the math stops resembling the stuff we see in everyday life.

Rest assured, the deep end will be a stop on our tour; in the meantime, there is still much groundwork to lay down before we can sensibly talk about what it means. We should begin with a precise definition of *observation*; later on, we will define *physical laws*, and what it means for observations to *follow* those laws.

An *observation* is just a statement that something occurred, along with some context about the occurrence, like date and time, or any other way to categorize observations. In high school physics, we are usually taught to think about an *event* as specifying a location (as in the three parameters $x$, $y$, and $z$) and a time ($t$), which a quite adequate way of cataloguing a wide variety of experimental data. However, there are plenty of other parameters that might be relevant to an observation, like temperature, settings on various dials and knobs, and so on. Eventually, we just get used to thinking in terms of some unspecified number of generic coordinates; when we really want to emphasize this, we use a letter like $q$ instead of $x$, which usually we reserve specifically for quantities that can be thought of as space/time coordinates ^[From now on, I will just write "spacetime", because it is never too early to start treating them together!].

It turns out that to make sense of physics, one of the parameters we will need to take into account is the *observational scale*. We usually think about it as a distance: quantum mechanics at atomic distances, classical mechanics at "human" distances, and general relativity at astronomical distances. It turns out we can measure it as an energy, too; that will make more sense later on. For now, we will just give it the symbol $\beta$, and remark that it has a critical role in describing the hierarchical structure we see around us: solar systems made of planets, planets made of atoms, and atoms made of freaky gizmos that only get freakier the further down we look.

## Experimental Control and Statistical Effects

(Define physical laws here)

Of course, no measuring device is perfect, and if anyone shows you data that perfectly agrees with the theory, they are showing you *fake data*. The difference between predicted and actual values is given an unnecessarily rude name (*error*) and it comes in two flavors: *systematic* and *random*.

To no one's surprise, having a degree does not prevent [silly mistakes](https://en.wikipedia.org/wiki/Faster-than-light_neutrino_anomaly), and much effort goes into preventing or accounting for *systematic* errors: anything that can be explained by things like an incorrectly calibrated laser, a programming error, or bad math. The correct way to prevent systematic error is to get as many people as possible to repeat your results in as many different ways they can think of; this is why science is never a solo activity. We will treat the problem of systematic error as "solved by teamwork" and put it out of our minds.

Even in the impossible universe where we can completely account for every single systematic error, we still can't measure things perfectly! Any remaining difference is called *random* error, but really we know where it comes from: it's just everything we didn't bother to include in our formulas, like tiny earthquakes and rounding errors in the data. The nice thing about this stuff is that if we do an experiment over and over again, as carefully as possible, we can treat these errors as *random variables*, in a precise mathematical sense that lets us make sense of the underlying pattern. In this sense, random error is "solved by repetition".

Taken together, we will refer to an experiment that has been reproduced an infinite number of times in an infinite number of different ways as "perfectly controlled". Obviously, this is not a practically achievable goal in a real laboratory. But we shouldn't lose heart! The patterns we're looking for are still there; we just have to work a little harder to spot them.

Now that we have assured ourselves that a mathematical formulation of physics is meaningful, we can actually go ahead and get down to it.
