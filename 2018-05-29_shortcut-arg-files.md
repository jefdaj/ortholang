---
title: Add .args files to all ShortCut functions
...

This is an idea I've been thinking about for a while but never had the time to dig in and attempt before my committee meeting.

`ShortCut.Core.Compile.Each` creates `.args` files to get around the fact that Shake needs to know all the input files of an `Action` at "`Rules`-time".
What if I added the `.args` step to everything else too?

Rationale
---------

In Shake, you can't map over a list of files without:

1. knowing the files beforehand
2. making them match a unique pattern and writing the rule to match all files with that pattern
3. telling it which file to read to get them later

Number 3 turned out least burdensome, although I tried 2 once too.
It works for mapping, but has created a fragile system where the mapped paths have to match their non-mapped equivalents exactly or hash-based deduplication fails.

Right now, ShortCut handles code like this:

1. parse text to expressions
2. compile expressions to rules
3. execute actions based on the rules

With the new step added, it would:

1. parse text to expressions
2. compile expressions to rules *for generating .args files*
3. execute actions to generate `.args` files
4. use one main rule to hook up each `.args` file with its main action + output
5. run the actions that do everything

It's complicated, but maybe not overly so. I think it would actually improve several things:

* map functions (currently called `each`) could use the same mechanism as everything else, eliminating the hash issues
* Shake could auto-parallelize every real action, even if I generate the `.args` files sequentially
* the `.args` files would act like an additional log of how each function was called
* I *think* there could be any number of additional "`.args` -> result that's also a `.args` file" steps added in the middle
* the main rule for `.args` -> output would be a good place to look up the corresponding file in a second long-term tmpdir cache

That last one would help with the web demo; I could have one main cache and each `shortcut` instance would use tmpfiles from it when possible.

Sketch out changes
------------------

It seems like a large change, but one that should theoretically be straightforward.

The only confusing issue I expect so far is that I'll need to record the `Rules` and `Action` for each `CutFunction` separately.
Maybe I should make a `CutFunction2` to ease the transition? Or maybe that would complicate it and I should just power through the errors all at once.
If it's easy I would love to keep the `Action`s as typed as possible, but could give up and go back to `[CutPath]` if not.

Initial steps:

* See if you can type tie two fields in a Haskell record together based on a common type, like `(CutPath, CutPath, CutPath)` for now.
* Skim through all the relevant code and see if any show-stopping issues pop up
