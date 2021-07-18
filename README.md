# rcc
A C compiler written in Rust. I decided to do this project to consolidate my understanding of writing a compiler. I decided to write a compiler for C since it is not only one of my favourite languages, it is also simple language in the sense that it does not have too many features (this makes things easier for me). Although I say that C is a simple language, I do not intend (at the moment) to support every single feature of C, this compiler will support just a subset of C. It is not yet decided what features will be included or excluded in this particular subset. I am no expert when it comes to writing compilers, I am also not that well read on the subject, which means that the resulting compiler will probably not be that amazing. I will not be focused on producing the most optimised assembly code, instead I will merely try to ensure the correctness of the code for now. Once I have successfully implemented all the features I intend to implement, I will try to optimise the generated code. For now, I will try my best to deliver code of the best possible quality, so that as I get more knowledgeable about compiler writing it will be possible for me to improve the implementation of the compiler. I will also try my best to write as many tests as possible.

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

18/7/2021 - Implement structs.

## Caveats
- Return statements are required in main functions.
- Declarations in functions (in the local scope) are not supported yet.
- Assignation in declarations are not supported yet.
```
- Can only handle one level of indirection for now.

Instead, only the below is supported.

``` c
struct Person
{
    char name[50];
    float salary;
};
struct Person person1;
```

## Tests
To run all unit tests only
``` bash
cargo test --lib
```

To run all integration tests only
``` bash
cargo test --test '*'
```