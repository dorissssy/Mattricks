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
| ***Tester***           | Hang Yuan     |

&nbsp;&nbsp;&nbsp;&nbsp;*Names are ordered by last name alphabetically*

## Guidance
### Environment Setup
1. git pull repo

> **For running:**  
> 1. `ocamlbuild test_<phase>.native`, e.g. `ocamlbuild test_parse.native`   
> 2. `./test_<phase>.native < example.mc > example.out 2>$1`
> e.g. `./test_parse.native < example.mc > example.out 2>$1`
> or
> `./test_parse.native < example.mc`

> **For testing:**  
> 1. `chmod 777 ./test` give E permission to test script  
> 2. `./test [optional: -flag(s)]`  
>   `-c`: clean, clean the temp folder after the test complete  
>   `-p`: parse, only run syntax phase tests in /tests/parse  
>   `-s`: semant, only run semantics phase tests in /tests/semant  
>   `-a`: all, by default run all the tests in /tests/*  
