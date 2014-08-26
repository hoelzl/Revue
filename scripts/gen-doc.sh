#! /bin/sh

# Generate documentation using *Marginalia*.  Currently we generate
# from the Javascript sources, since *Marginalia* does not know about
# #+clj or #+cljs tags in CLJX files.

echo "Starting Leiningen.  This may take a moment."
lein do cljx once, marg -d doc -f revue.html -D \
"# Reversible User Experiences

*REVUE* is an interpreter/virtual machine that returns a trace of a
program\'s execution.  This trace can be used for visualizing the inner
workings of a program, animating algorithms, etc.

More precisely, \`step\`, the central function of *REVUE* interprets a
Scheme-like intermediate language by taking a program state consisting
of

* a form to be evaluated,
* an environment
* a store
* a continuation
* the value of the previous step

and returning a new state of the same form.  To evaluate the program,
we can then simply iterate the \`step\` function to obtain an infinite
sequence of states.  (Calling \`step\` with the final state of a
terminating computation simply returns the same state again.)  For
visualizing terminating algorithms, a convenience function \`interp\` is
provided that runs the VM to completion and returns the resulting
seqence of states."\
     target/classes/revue/util.cljs\
     target/classes/revue/mem.cljs\
     target/classes/revue/interpreter.cljs

