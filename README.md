# Mattricks

## Brief Introduction

Mattricks is an imperative programming language heavily focused on mathematical calculations similar to Matlab, with a particular focus on matrix calculations. Mattricks includes most mathematical symbols and syntax, so users do not need to include any math packages. Moreover, the mathematical syntaxes are designed to be easy to understand for users, so users without much prior knowledge of computer science will also be able to write their own calculations using our language. 

## Development Team

| Role                   | Name          |
|------------------------|---------------|
| ***Manager***          | Yuxin Yang    |
| ***System Architect*** | Yuanhan Tian  |
|                        | Weisheng Wang |
| ***Language Guru***    | Danny Hou     |
|                        | Jack Wang     |
| ***Tester***           | Yuhao Dong    |
|                        | Hang Yuan     |

&nbsp;&nbsp;&nbsp;&nbsp;*Names are ordered by last name alphabetically*


## File structure
```
Mattricks
│   README.md
│   Makefile
│   scanner.mll
│   ast.ml
│   parse.mly
│   sast.ml
│   semant.ml
|   irgen.ml
|   mattricks.ml
│   test_script
│   example.mc
│   Mattricks - Language Reference Manual
|   Final Project Report
│
└─── test_cases
    │   parse
    │   semant
    |   runnable
    │   test_parse.ml
    │   test_semant.ml
```

## Run and Test

> **Running:**  
> Use Makefile
> `make parse` or `make semant` or `make compiler` will automatically build and run with file `example.mc`. `example.out` will be generated.


> **Testing with test suite:**  
> 1. `chmod 777 ./test_script` give E permission to test script  
> 2. `make test`   
> If seen this error: all test cases failed, please change the line ending of all files under /test_cases/ from 'CRLF' to 'LF' to make script run    

## Features provided
- Declaration of variables.（int, float, bool, 1D ad 2D matrices）
- Assignment of variables using '='
- Arithmetic and logical operation
- Code blocks
- If statement
- While loop statement
- Accessing data in the matrices via subscript access.(Using array_id[idx] as an expression)
- Bind assignment
- Static scoping

## Features should be done but haven't done
- Type inference(similar to C++'s auto keyword and Go's := operator)
- Casting

