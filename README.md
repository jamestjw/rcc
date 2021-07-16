# rcc
A C compiler written in Rust. I decided to do this project to consolidate my understanding of writing a compiler. I decided to write a compiler for C since it is not only one of my favourite languages, it is also simple language in the sense that it does not have too many features (this makes things easier for me). Although I say that C is a simple language, I do not intend (at the moment) to support every single feature of C, this compiler will support just a subset of C. It is not yet decided what features will be included or excluded in this particular subset. I am no expert when it comes to writing compilers, I am also not that well read on the subject, which means that the resulting compiler will probably not be that amazing. However, I decided to try my best to deliver code of the best possible quality, so that as I get more knowledgeable about compiler writing it will be possible for me to improve the implementation of the compiler. I will also try my best to write as many tests as possible.

## Progress
I will try to document the progress of this project as best as I can. I will most probably only be noting down important milestones.

29/6/2021 - Started the project. Implemented a simple scanner that supports a limited set of tokens.

30/6/2021 - Implemented a Pratt parser for expression parsing.

3/7/2021 - Implemented code generator and set up integration tests.

5/7/2021 - Implemented support for declaration and assignation to global variables.

7/7/2021 - Support the parsing of function declarations and return statements. Added minimal support for function calls with no arguments.

10/7/2021 - Support calling functions with multiple parameters according to x86-64 C calling convention.

14/7/2021 - Add support for prefix operators. Implement pointer types and support taking addresses of variables and the dereferencing of pointers. Introduced string literals.

17/7/2021 - Implemented arrays

## Tests
To run all unit tests only
``` bash
cargo test --lib
```

To run all integration tests only
``` bash
cargo test --test '*'
```