# ShortCut

ShortCut is a simple scripting language for making phylogenomic "cuts". It aims
to eliminate a few of the pain points I've run into while doing them manually:

* Keeping track of a large number of temporary files
* Running slight variations on the same CPU-intensive code over and over
* Coming back to check on a long-running pipeline, only to find that it's
  stopped because I gave a wrong arguments to one of the later steps
* Explaining the pipeline accurately without resorting to large flowcharts
  or cryptic makefiles

Beyond those specific improvements, the goal is to create a pleasant
environment that removes as much of the "busywork" as possible so you can focus
on biology instead.

<!-- TODO add stuff about reproducibility -->
<!-- TODO add a section about code design -->

## Install

I do all my coding these days in reproducible per-project build environments
set up by [Nix](https://nixos.org/nix/). Use `nix-build` to build the entire
project, then `./result/bin/shortcut` to launch the interpreter. You can also
use `nix-shell` to enter a development shell for working on the interpreter
code, or `cd scripts && nix-shell` to work on one of the scripts called by it.

## Learn the language

ShortCut is very easy to learn compared to "real" programming languages like
Python or R, but it also executes code in a unique way that may take some
getting used to. So read through this short tutorial, try it out, and email me
if/when you find things that don't make sense or could be clearer!

### Interpreter

You can run ShortCut scripts ("cuts") the same way you run code in other
scripting languages, by passing a filename to the interpreter: `shortcut
my.cut`. After probably a long time computing, it will print results. 

But most of the time you won't be running a finished script. Instead you'll
start in interactive mode by typing just `shortcut`:

~~~
Welcome to the ShortCut interpreter!
Type :help for a list of the available commands.
shortcut >>
~~~

As the `:help` says, from there you can load a script and play around with it.
This is one I've been working on to find likely photosystem II assembly
factors:

~~~
shortcut >> :load cuts/psII.cut 
shortcut >> :show
plastidcut = load_genes "genes/tair-plastidcut2.faa" -> genes
knowngenes = load_genes "genes/tair-known-ps2genes.faa" -> genes
knowngenomes = load_genomes "lists/shi-falkowski-cyanos.txt" -> genomes
cutoff = worst_best_evalue knowngenes knowngenomes -> number
ucyna = load_genomes "lists/ucyna-genomes.txt" -> genomes
othercyanos = load_genomes "lists/ncbi-cyanos.txt" + load_genomes "lists/img-cyanos.txt" - ucyna -> genomes
goodcyanos = filter_genomes othercyanos knowngenes 1.0e-15 -> genomes
ingoodcyanos = filter_genes plastidcut goodcyanos 1.0e-12 -> genes
inucyna = filter_genes plastidcut ucyna 0.1 -> genes
psIIcut = ingoodcyanos - inucyna -> genes
~~~

By the end of the tutorial you should understand everything going on in it, and
be able to write you own, similar cuts for any biological process you're
interested in. But first, we should start from the basics.

#### Part 1: Commands

<!-- TODO make this later, after variables and stuff -->

ShortCut works mostly the same whether you're running a script or loading it
interactively, but the interactive version also has some extra commands
starting with colons (`:`). The most important is `:help`, which explains the
others:

~~~
shortcut >> :help
You can type or paste ShortCut code here to run it, same as in a script.
There are also some extra commands:

:help  to print this help text
:load  to load a script (same as typing the file contents)
:write to write the current script to a file
:drop  to discard the current script and start fresh
:quit  to discard the current script and exit the interpreter
:type  to print the type of an expression
:show  to print an expression along with its type
:!     to run the rest of the line as a shell command
shortcut >> 
~~~

Let's clear out the `psII.cut` script so we can start over.

~~~
shortcut >> :drop
shortcut >> :show
shortcut >>
~~~

After `:drop`ping all the loaded variables, there's nothing left to `:show`.
Now, let's start with the 

~~~
~~~

### Math

The simplest thing you can do in ShortCut is probably math. It works more
or less like you expect.

~~~
shortcut >> 1 + 1
2.0
shortcut >> 2 * 3.45234e-8
6.90468e-8
shortcut >> 2 * 3.45234e-8 - 3.5
-3.4999999309532
shortcut >>
~~~

Unlike in some languages, there’s only one type of number. It’s called
“number”. You can write integers, decimals, or scientific notation. The
only gotcha is that math doesn’t follow normal order of operations—instead,
everything is just left to right. For example, `3 + 1 / 2` is `2`, not
`3.5`. PEMDAS could be added fairly easily, but I haven’t done so it
because the `+` and `-` functions are also used with sets, and I think it
would be confusing if they switched behavior depending on the type you call
them with. More on that later. Most phylogenomic cuts won’t involve long
equations, and if you do need one you can use parentheses.

### Lazy evaluation

Let’s try assigning a variable.

~~~
shortcut >> var1 = 1 + 1
shortcut >>
~~~

If you do `:show var1`, what do you expect it will say? Most people would
guess `2.0`. But actually:

~~~
shortcut >> :show var1
1.0 + 1.0 -> number
~~~

This is where ShortCut starts to get a little weird. It has “lazy” evaluation,
which means nothing is computed until you ask to see the result.  When we
created `var1` it just said “OK, now var1 is defined”. Now you’ve asked “How is
var1 defined?”, and it says “var1 is the result of 1 + 1, which will be
a number”.

Silly, right? It makes more sense when you consider expensive computations.
If `var1` were a huge table of 10,000,000 BLAST results, you might want to
finish setting up the rest of the script, then go home and let it run
everything at once overnight. But since we know `1+1` is easy, we’ll go
ahead and tell it “Yes, I really do want to know what the number is”.

~~~
shortcut >> var1
2.0
shortcut >>
~~~

You can call any function with the results of previous functions. This doesn’t
force evaluation. When you finally ask to see the result of one of them,
ShortCut will go back and calculate everything needed in the right order. It
will also do steps that don’t depend on each other in parallel.

If you’ve ever written a makefile, assigning the variable is similar to writing
a rule that will generate the `var1` file, and asking for the answer is like
running `make var1`. As you’ll see in a few minutes, this is very close to how
ShortCut works internally.

### Types

Before we get to that though, let’s consider the situation where you have
a series of expensive computations you want to run overnight. How do you know
your script won’t crash at some point because you gave one function a gene name
when it was expecting a number? That’s what the type system is for. As you
write your code, ShortCut checks that every function is called with values of
the right type. If you try to call the `+` function with a number and a string,
it will refuse:

~~~
shortcut >> var2 = 1 + "bob"
Wrong argument types for the function '+'.
  Need: number, number
  Got:  number, string

shortcut >> 
~~~

The combination of lazy evaluation and a strong type system let you know that
your entire script is reasonable before you run it. Of course it might still
fail for reasons that can’t be known beforehand, like maybe one of your BLAST
searches won’t return any hits. But at least it will catch the obvious
mistakes.

Besides numbers and strings the available types are: gene names, genome
names, blast hit tables, FASTA files (nucleic or amino acid), and sets of
any of those. I plan to add phylogenetic trees (gene and species) soon too.

### Scripts and Temporary files

There's one more important difference between ShortCut and other languages you
might be used to. Well, two really: functions correspond to programs, and
variables correspond to files. ShortCut itself doesn't do much computing. It
does have built-in math and set operations, but everything else is a matter of
calling other programs in the right order with the right filenames. Consider
this line:

~~~
ingoodcyanos = filter_genes plastidcut goodcyanos 1e-12
~~~

It calls the function `filter_genes` with three arguments and assigns the
result to the variable `ingoodcyanos`. But really, there's a script
`filter_genes.R` that gets called with three temporary files and creates
a fourth. Everything gets a temporary file! By default, they go in the
`_shortcut` folder. When we asked ShortCut to calculate `var1` above, it
created a text file `_shortcut/var1.num` with "2.0" in it. This function call
will create `_shortcut/ingoodcyanos.genes`, which will contain a list of gene
names. There are lots of other unnamed temporary files too, for example there's 

### Examples

### Developing Shortcut

You can `nix-build` the main package or any of the top-level subdirectories. I
find that to be annoyingly slow for Haskell packages though, because it
recompiles them from scratch each time. So I normally keep `stack test
--file-watch` open in one terminal to do incremental builds + tests, and `stack
ghci` in another for playing around with the types. Running `nix-shell`, then
`cabal repl` would work for that too.

### Reference
