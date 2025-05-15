# The AI of the Tiger
The goal of this project is to develop a static analyzer for the Tiger
language based on Abstract Interpretation.  Its objective is to
determine **statically** whether an array access is safe.  It must be
sound/correct/conservative (call it what you want), meaning that if it
classifies an array access as safe, it must be safe under all
circumstances (i.e. in all possible executions of the
program). Otherwise, the access should be considered unsafe.

# Language
This project is based on the [Tiger
language](https://assignments.lrde.epita.fr/reference_manual/tiger_language_reference_manual/semantics/semantics.html)
as you know with few differences: for pedagogical reasons, we restrict
ourselves to analyzing a subset of Tiger.  In particular, the version
we use does not handle:

- Function definitions (but function calls are allowed)
- Objects
- Records
- Nil

We further restrict ourselves to the following primitives:
- print
- print_int
- concat
- range*

Finally, to make the analysis more challenging, we have added a source of
non determinism inside the language in the form of the primitive:

````
/* range: int * int -> int */
range(low,sup)
```` 

Which samples randomly (and uniformly) an integer between `low` and
`sup` (inclusive)

# Where to start?
Try compiling the project. If you have dune and OCaml (> 5.0) installed, a
simple make should do the job.
If compilation is successful, launch the main executable on an example file.

- Usage `./src/concrete/tiger_concrete.exe tests/interpreter/interp00.tig`

This will by default parse the file, interpret it and exit the program.

If everything was successful, this should print 1.

It is also possible to print the constructed AST of the program using the `--show-ast` option as in the following example: 
- Usage `./src/concrete/tiger_concrete.exe tests/interpreter/interp00.tig --show-ast`

### Overview of this repository
- The `tests/` directory contains `.tig` files that you'll work with.
  * The subdirectory `good/` contain programs with **only valid array accesses**. Your goal is to make your analysis **precise enough** to recognize them as valid as often as possible.
  * The subdirectory `bad/` contain programs with **only invalid array access**. Your goal is to keep your analysis **sound**, ensuring that these access are **not mistakenly accepted** as valid.
  * The subdirectory `interpreter/` contains simple Tiger programs not necessarilly with array accesses. Their purpose is to let you observe how your analysis behaves on small, readable examples.
- The `src` directory contains the sources  

# Your job
- You will go through the steps below to build both an interpreter and an analyzer for the tiger language.
- You will run the tests (`make test` to run the whole testsuite. Or use the more specific makefile targets for each step)
- If a "Not implemented yet" exception is raised, try yo understand where it comes from and implement the missing feature

# Step 1 : Interpretation
The goal of this first part is to implement an *interpreter* for the
Tiger language.

Try the command :
`./src/concrete/tiger_concrete.exe --show-ast tests/interpreter/interp00.tig`

**Code to read:**
- `src/ast/ast.ml` : Describes the language through
the definition of its abstract syntax tree (AST). Each element of the
language is represented as data types, enriched with location
information to allow accurate error tracking in the source
code. Expressions (expr) cover the usual constructs of the tiger
language: constants, variables, operations, control strutures (if,
while, etc.), function calls, and initializations of arrays.
Lvalues ​(assignable values) allow assignment targets to be
modeled. Declarations (chunks) encapsulate the high-level blocks of a
program, whether they are type declarations, variables, or
expressions. Finally, a complete program is seen as a list of these
blocks.

- `src/interpreter/value.ml`: Defines the values ​​ handled at runtime
by the interpreter, grouped into a sum type that includes integers,
strings, arrays and void. It also provides dynamic conversion
functions (cast_int, cast_array) to check that a value is of the
expected type, with error reporting in case of failure.`

**Code to write:**
- `src/lib/interpreter/interpreter.ml`
This code implements a functional interpreter, with pure recursive
expression evaluation. The program state is modeled as an immutable
structure containing an environment of variables and a table of
functions. The implementation assumes well-typedness and more
generally, it is assumed that programs are well-formed.

Some hints:
  - Scopes are manually managed via a stack of environments (see env.ml)
  - Assignable expressions (lvalues) can be read or written
  - Native functions are integrated directly into the initial environment.
  - Since we suppose that programs are well typed, we dont need to deal with types i.e. type declarations have no effect

**Tests**
run `make interpretertest` This will look in the tests/interpreter/
directory, parse and evaluate each .tig Tiger file, compare the
evaluation result to a .print reference file and report whether the
interpreter output matches the expected result.
