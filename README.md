# SymmLang
Interpreter for my language (Symm), implemented in Haskell

What does the language allow:
1. three types -> Int, Bool, String, FunT
2. literals, arithmetics, comparison
3. variables, variable assignment
4. read-only variables
5. static attachment
6. for loop, while loop, if
7. functions and procedures
8. nested functions with static attachment
9. recursion
10. Print function
11. error handling

In good/ directory there are examples of good usage, and in a bad/ directory there are examples of bad usage. 

How to run the interpreter:
1. make
2. ./interpreter "program-name"

Cleaning files:
1. make clean

ATTENTION: Files in directory Grammar are partially auto generated with usage of bnfc program, based on grammar saved in grammar.cf file. Don't delete these files!

Language description:
* To create a variable, there is a Let keyword
* Remember about ';' character at the end of each operation (including block operations like "while () {};")
* In the Symm language instructions are read C++ likewise (from up to bottom, from the left to the right)
* Variables are attached statically, so after leaving the scope they can no longer be accessed
* You CAN'T cover (Rust likewise) already existing variable/function name
* Other than common variable types, there is a FunT type that stores functions
* You can pass a function as an argument!
* There is support for String and String, String and Int, String and Bool addition
* There is support for equal comparison (== and !=) for comparing Strings with Strings and Bools with Bools

Uncommon syntax:
* In case of declaring a variable without initializing it, the default value will be assigned (0 for Int, False for Bool, and "" aka. empty string for String)
* The FunT type does not have a default value, therefore in case of trying to create an uninitialized variable of type FunT, the Error will be thrown
* The language supports read-only variables for Int, Bool, and String types. To create a read-only variable, there is a Read keyword
* As before, read-only is not supported for FunT type
* There is no Void type
* There are two different if expressions.
   * One for only if: if (expression) {};
   * Second for if/else: eif (expression) {} else {};
* For loop looks as follows: for (i in 1..10) {};. That loop would loop from i=1 to i<10 with jump 1. The variable i is read-only.
* When declaring a function, you only assign arguments names (Python likewise). Then the function can take different types as arguments.
* Type checking is dynamic. It means that as long as the operations are possible, there won't be any Error thrown.
* Builtin Print function (Python likewise)
* Print supports Strings, Ints and Bools - normal and read-only.
