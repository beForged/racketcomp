# Small Scheme

 Scheme/Racket Compiler to x86 with a C runtime, written in pure Haskell

## About
This is a small compiler written in pure haskell (except for requisite IO to deal with files) to run a subset of Racket/Scheme. 

Currently supported features include:

- Lambdas
- Lists
- Boxes

To do list:

- letrec
- Strings
- Pattern Matching
- Multiple files
- Header support
- Macros
- Consistent lambda handling internally
- Type Checking
- Optimization of generated assembly
- Separation of generated files like in a target or tmp/ folder



## Running / Requirements
You will need ghc (preferably installed via stack), gcc and nasm. Racket would also be helpful.

Currently only supports singular files, in order to compile, run 
`make inputfile.run`
 in the main folder where imputfile is a racket/scheme file.
 
 This will generate an `inputfile.run`  that can be run by your system.
 
 The Makefile may need to be adjusted by changing the `%.o` rule to use `$(format)` instead of elf64 if you are not on linux. In addition, if using cabal or stack, you may need to remove the `-dynamic` flag in the makefile if it is giving you issues. 
 
In order to generate x86 code from an arbitrary .rkt file, you can run `stack exec inputfile.rkt outputfile.s` where outputfile.s can be any name you want really. This will generate a file of assembly. Parsed data can be generated using the `parseFile` function in Compile_file.hs which will generate a .par file with the parsed data.

dont use test.sh, it is just a reference for the final test setup - run `stack test` to run the test suite

canonical.sh should run racket on each rkt file in features and generate ref (erence) files. 

