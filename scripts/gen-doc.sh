#! /bin/sh

# Generate documentation using *Marginalia*.  Currently we generate
# from the Javascript sources, since *Marginalia* does not know about
# #+clj or #+cljs tags in CLJX files.

echo "Starting Leiningen.  This may take a moment."
lein do cljx once, marg -d doc -f revue.html -D \
"# Reversible User Experiences

*REVUE* is a compiler/virtual machine combination that returns a trace
of a program's execution.  This trace can be used for visualizing the
inner workings of a program, animating algorithms, etc.

More precisely, *REVUE* contains a compiler for a simple Lisp dialect
called *Riley* (Revue Intermediate Language, ey), that generates
byte-code for a simple stack-based VM and an implementation of the VM
in purely functional form, i.e., the VM is implemented as a function
\`step\` that takes a state as input and generates a new state as
output The state transformation is implemented using efficient
persistent data structures, so that it is feasible to retain the
execution trace of the program.  Eventually compilers for other input
languages will be provided.

To evaluate the program, we then simply iterate the \`step\` function
to obtain an infinite sequence of states.  (Calling \`step\` with the
final state of a terminating computation simply returns the same state
again.)"\
     target/classes/revue/util.cljs\
     target/classes/revue/mem.cljs\
     target/classes/revue/vm.cljs\
     target/classes/revue/riley.cljs

