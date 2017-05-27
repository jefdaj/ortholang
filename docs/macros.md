substitution macro
==================

Macros are mainly intended to support my idea for "universal bootstrapping" of
ShortCut programs (there's probably a better word for it). I want to be able to
write functions to assess the robustness of results using brute force rather
than statistics, because I'm lazy! Functions that will answer questions like:

* "Which of my results would change if I started from a different list of
   model genomes?"

* "If I removed the known PSII assembly factors one at a time, which of them
   would be rediscovered in my results?"

* "Does adding more genomes increase the number of known assembly factors
   I rediscover, or is it just a waste of CPU cycles?"

These and other similar questions should be answerable using only a moderately
complicated algorithm:

1. Permute the "independent" list into a list of lists (there will be
   various functions to do this in different ways)

2. Calculate the "dependent" list for each of the permuted inputs

3. Summarize results back down to a single value (there will be a couple ways
   to do this, maybe by keeping or removing the common elements, or just
   counting them as a score of robustness
