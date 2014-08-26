#! /bin/sh

# Generate documentation using *Marginalia*.  Currently we generate
# from the Javascript sources, since *Marginalia* does not know about
# #+clj or #+cljs tags in CLJX files.

echo "Starting Leiningen.  This may take a moment."
lein do cljx once, marg -d doc -f revue.html\
     target/classes/revue/util.cljs\
     target/classes/revue/mem.cljs\
     target/classes/revue/interpreter.cljs

