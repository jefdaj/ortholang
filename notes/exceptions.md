Exception handling
==================

This is one of the more complicated and annoying parts of Haskell, made worse
by all ShortCut's system commands and tempfiles. I think the best way forward
for now is to ignore best practices and just:

* Abort the whole program with `error` any time there's a problem that can't
	easily be recovered from in a parser, compiler, or system call
* Catch those exceptions (and any others) in the REPL and print them,
  then keep going from the previous state

That leaves only two things that need doing:

#. Figure out how to catch all errors in the REPL
#. Figure out how to cancel any SLURM jobs/system calls
   (whether in the REPL or a script)
