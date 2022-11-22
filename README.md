# Mattricks

## Brief Introduction

Mattricks is an imperative programming language heavily focused on mathematical calculations similar to Matlab, with a particular focus on matrix calculations. In Mattricks, we will include most mathematical symbols and syntax, so users do not need to include any math packages. Moreover, the mathematical syntaxes are designed to be easy to understand for users, so users without much prior knowledge of computer science will also be able to write their own calculations using our language. Features like higher-order functions enable users to customize and package their calculations into functions and matrices easily and could combine and reuse these functions with more convenience. It also provides more readability so that users can easily understand the programmers' intentions.

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
│   test
│   test_parse.ml
│   test_semant.ml
│   Mattricks - Language Reference Manual
│
└─── test_cases
    │   parse
    │   semant
```

## Run and Test

> **For running:**  
> 1. Use Makefile. `make parse` or `make semant` will automatically build and run with file `example.mc`. `example.out` will be generated as output
> 2. Manually build and test: `ocamlbuild test_<phase>.native`, e.g. `ocamlbuild test_parse.native`
>    then `./test_<phase>.native < example.mc > example.out`, e.g. `./test_parse.native < example.mc > example.out`

> **For testing with test suite:**  
> 1. `chmod 777 ./test` give E permission to test script  
> 2. `./test [optional: -flag(s)]`  
>   `-c`: clean, clean the temp folder after the test complete  
>   `-p`: parse, only run syntax phase tests in /tests/parse  
>   `-s`: semant, only run semantics phase tests in /tests/semant  
>   `-a`: all, by default run all the tests in /tests/*  
>   `-f:`: run single test case, followed by the test file path, e.g. `-f parse/pass_bool`   
>   If seen this error: `-bash: ./test.sh: /bin/bash^M: bad interpreter: No such file or directory` or all test cases failed, please change the line ending of script/test.mc files from 'CRLF' to 'LF' to make script run    

## Features provided
- Declaration of variables.（int, float, bool, 1-2-3D matrices）
- Assignment of variables using '='
- Code blocks.
- If statement
- While loop statement
- Accessing data in the matrices via subscript access.(Using array_id[idx] as an expression)
- Bind assignment.
- Static scoping.

## Features should be done but haven't done
- Type inference(similar to C++'s auto keyword and Go's := operator)
- Assignment of matrices' element via subscript operator. (a[x] = y)
- More logical operators.
- Multiply and division.

## Hello World Front-End Contribution
**Weisheng Wang**(ww2609): I participated in group meetings to plan short term goals and distribute workloads; I did pair programming with Yuhao Dong on adding more rules in ast, parser and scanner files; I reused microc's example code to allow function definition; I also designed the Mat type and realized its rules(with Yuhao Dong's help on debugging 2d matrix rules).  
**Yuanhan Tian**(yt2825): I reused some of the microc's example code and implemented part of the ast , the semantics checker and sast. I discussed the plans with the group members in the Zoom meetings. I collaborated with Danny Hou to implement part of the scanner.  
**Yuhao Dong**(yd2626): I participated in group meetings to discuss the team plans, individual roles, and difficulties we faced. I worked with Weisheng Wang together on adding and modifying rules in ast, parser, and scanner files according to our own language features. I also worked with Weisheng Wang to fix the 2d matrix parsing rules to make it work as designed and to eliminate the conflicts.  
**Yuxin Yang**(yy3277): 1. Created test cases for testing the parser of the program matching with the features mentioned in LRM; 2. Organized and managed this repository and group meetings 3. Helped identify some issues and existing bugs in the grammar; 4. Updated LRM  
**Hang Yuan**(hy2784): 1. Helped define the part of the proposal and LRM, proofread and finalize it; 2. Designed and implemented the test automation script; 3. Wrote all test cases for semantics phase; 4. Found out grammar ambiguity, created Github Issues for sytax/semantics bug; 5. Helped debug a small portion of the scanner and parser.  
**Danny Hou**(dh3034): I discussed our plan with team memebrs during group meetings and collaborated with Yuanhan to work on the tokens and rules of our scanner reusing some of the microc's example.  
**Jack Wang**(yw4014): I parcipated in dicussions on implementation, ast, and semant. The discussions finalized key items for submission. 
