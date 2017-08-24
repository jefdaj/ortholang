Exceptions
==========

This is one of the more complicated and annoying parts of Haskell, made worse
by all ShortCut's system commands and tempfiles. I think the best and simplest
way forward is to break the codebase into "pure Haskell code" and "non-pure
Haskell code + everything else". The pure code can use typed errors, and the
rest regular try/catch. In more detail, these are the behaviors I want:

#. The parser uses `Parsec` error handling and never throws a raw `error`
#. The compiler never throws a raw error either... is there `Shake` error handling?
   If not I make my own.
#. The REPL catches all `IO` errors and throws them away after printing a message
#. Compilation + evaluation + running code can be canceled with `Ctrl-C`,
   dropping back to the REPL with no ill effects
#. `Ctrl-C` in the REPL itself could quit the program, but after seeing that `ghci`
   ignores it I actually like that approach too.
#. When running a script though, any IO error cancels everything
   (Should that apply to individual jobs?)

Some remaining questions before I try to get that working:

* Should I use `Haskeline`'s `Ctrl-C` handling, or treat that as just another `IO` error?
* Do I ever care about the types of `IO` exceptions, or just that there was one?
* Is it worth cleaning up the parser and compiler of `error` calls, or should
  I just wrap them to save time?
* Is there any need to delete partial files in case of system call errors,
  or does `Shake` already do that?
* Should I try to incorporate the `safe-exceptions` library, or roll my own stuff?
* Should I define my own `Exception` datatypes?
* How can I cancel any SLURM jobs already launched when there's a ShortCut error?
* Should I replace the convention of "`Nothing` exits the REPL" with an exception?

If I do define my own types, they could be pretty simple:

* `ParseError`
* `CompileError`
* `EvalError`
* `QuitRepl`
