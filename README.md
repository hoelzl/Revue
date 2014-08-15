# *REVUE*

A reversible virtual machine, based on the VM from Peter Norvig's book
"Paradigms of Artificial Intelligence Programming"

*REVUE* is a virtual machine that allows you to move forward and
backwards through the program execution, similar to reversible
debuggers.  It is intended to serve as a component in a system for
visualizing algorithms.  *REVUE* is written in Clojure/ClojureScript
and can run on both the Java Virtual Machine and on JavaScript
runtimes such as browsers or node.js.

## Usage

The build system for *REVUE* is set up so that you can build stand
alone libraries using the *Leiningen* build tool for Clojure; it also
supports interactive development using any nREPL-compatible
development environment, such as Cider for Emacs.

### Installation

Currently no binary packages are available.  To obtain the souce code,
clone the GitHub repository at `git@github.com:hoelzl/Revue.git`.  To
build a jar archive for embedding *REVUE* into Java applications, type
`lein jar`; to build a jar archive containing *REVUE* and all its
dependencies, type `lein uberjar`.

To build and run the test suite for both the JVM and the JavaScript
versions, run `lein cleantest`; the tests are written using the
double-check framework for property-based testing and generate a
number of largish data structures to convert to and from the VMs
internal format, so running the test suite might take a few minutes.
To run only the Java or JavaScript based tests, type `lein jtest` or
`lein jstest` into the shell.

### Developing with Emacs

Since the Clojure "offline" build process is very slow and cumbersome,
the recommended method of hacking *REVUE* is by using an editor with
integrated REPL.  If you have set up Cider for Emacs, you can simply
load one of the source files an type `C-c M-j` or `M-x cider-jack-in`
and a Clojure REPL will start up.  To start a ClojureScript REPL,
simply type `(cemerick.austin.repls/exec)` in the Clojure REPL, and a
ClojureScript-based REPL will start up.

All sources are translated into Clojure/ClojureScript with the `cljx`
tool and can be found in subdirectories of `src/cljx` or `test/cljx`.

### Embedding and Running *REVUE*

The interface to *REVUE* will be described once the relevant parts
have been implemented.

## License

Copyright © 2014 Matthias Hölzl

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.

TODO: Add MIT as second license option

