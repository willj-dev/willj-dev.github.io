------------------------
title: Implementation Ideas
blurb: >
  Little widgets and knick knacks that would help make MU transition seamless, but aren't really worth mentioning anywhere else
------------------------

PM data sync
*************************************************

Utility which syncs secure personal data in the MU with PM data storage (gcloud, icloud, …)

Council business tracker
*************************************************

Live updates on what is under consideration, who has the floor, whether there's a quorum…

Come to think of it, this could have little tooltips to show what motions would currently be in order, information about parliamentary procedure…

GC Rules of Order
*************************************************

Like it says, for each of the MUG councils. In "contemporary style" (modern language, organized like web docs, ...) but make it clear that it's basically just specific applications of RRO

RRO style and organization (especially copious notes) as the standard MU documentation style?

Time
*************************************************

One of the advantages of starting from scratch and slowly building our membership, rather than attempting to manage the transition over an entire population at once, is that we can spruce up a lot of systems that have not aged very well.

One of these is time: leap years, inconsistent numbering...of course, we should learn our lesson from the French Revolution and not try to go too overboard.

A "binary timestamp" is an unsigned binary integer of some specified size along with enough reference data to define an epoch:

- A reference event to set the start point of the epoch
- A reference period to set the "tick length" of each timestamp increment that can be represented in the given number of bits

We will define the Proto-Utopian Epoch to start at the same time as the Unix Epoch (e.g. 1970-01-01T00:00.000Z). Eventually, we hope, there will come a time when we can mark the start of the official First Utopian Epoch :)

As for time scale, let us start with a 128-bit integer and see how much that will get us. First, we can look for some obvious boundaries. Each increment of one Planck time within a single second could be labeled with 144 bits (that is, 21/tp), which is okay because once we’re poking around at that scale, we won’t be limited by such trivia as the number of bits we can afford to spend on timestamps. People are pretty used to using 4 decimal digits to write a year, so let’s say we want to at least be able to count somewhere around that order of years in our epoch. 8192=213, but 13 is an ugly number, so let’s round up to a nice power of 2 like 16. Devoting 16 bits to a year leaves us with 112 bits to divide within each year. This means that our smallest division of time will divide one Earth orbital period into 2112 segments, each equivalent to about 6E-27 seconds, which should be quite adequate for a while. We can now define a conversion constant between Unix milliseconds and 128-bit PUE time:

kPUE128:Ums=1.64531091023z

Multiplying Unix millis by this number will give the current "PUE128 time". Coincidentally, this happens to be on the same order as Avogadro’s number, but I’m pretty sure that’s just a fluke.

This defines a universal timeline suitable for computers, but doesn’t really satisfy important "real-life" needs that humans are accustomed to. We will need two more time-shaped systems: a solar calendar tied to orbital reference points (such as the two polar solstices each year), which is divided into 12 months, where each month is made of 4 weeks of 7 days. Unfortunately, Earth doesn’t rotate a whole number of times around its own axis in each orbit; if each day is 24 hours, this means that we will need to 

- Define one day as the amount of time it takes the earth to move through an orbit according to some published source.
- Then one year is 360 days, with 12 months of 30 days. Divide each day evenly into 24h*60m*60s. 
- How long is that second compared to Gregorian? A bit larger, by about 3/20 of a contemporary second. So, doable.
- This is enough to construct a universal timeline tied to a specific reference event and scale. 
