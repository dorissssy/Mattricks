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

## Run and Test

> **For running:**  
> 1. `ocamlbuild test_<phase>.native`, e.g. `ocamlbuild test_parse.native`
> 2. `./test_<phase>.native < example.mc > example.out`
> e.g. `./test_parse.native < example.mc > example.out`
> or
> `./test_parse.native < example.mc`

> **For testing:**  
> 1. `chmod 777 ./test` give E permission to test script  
> 2. `./test [optional: -flag(s)]`  
>   `-c`: clean, clean the temp folder after the test complete  
>   `-p`: parse, only run syntax phase tests in /tests/parse  
>   `-s`: semant, only run semantics phase tests in /tests/semant  
>   `-a`: all, by default run all the tests in /tests/*  
>   `-f`: run single test case, following by file path, e.g. `*parse/pass_bool*` (you can put any char in place of *)  
> If seen this error: `-bash: ./test.sh: /bin/bash^M: bad interpreter: No such file or directory` or all test cases failed, please change the line ending of script/test.mc files from 'CRLF' to 'LF' to make script run    

## Features provided
- 
- 
- 

## Features should be done but haven't done
- 
- 

## Hello World Front-End Contribution
**Weisheng Wang**(ww2609): I participated in group meetings to plan short term goals and distribute workloads; I did pair programming with Yuhao Dong on adding more rules in ast, parser and scanner files; I reused microc's example code to allow function definition; I also designed the Mat type and realized its rules(with Yuhao Dong's help on debugging 2d matrix rules).  
