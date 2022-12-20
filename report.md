# COMS 4115 Report

- Group Name: Mattricks
- Group Members: Weisheng Wang, Hang Yuan, Yuhao Dong, Yuxin Yang, Danny Hou, Yuanhan Tian, Jack Wang

## Introduction
Mattricks is an imperative programming language focused on mathematical 
calculations similar to Matlab, with a particular focus on matrix calculation. 
The motivation behind our language is that matrix calculation is widely used in linear algebra and machine learning models, and there is no existing language that provides the perfect solution for such calculation. 
Existing languages either need the support of additional libraries or their complicated grammar makes it less friendly for new users to perform matrix calculation. Therefore, we want to design a language that provides users with the ability to perform matrix calculations with peace of mind. 
In Mattricks, we will include mathematical symbols and syntax, so users do not need to install any math packages. 
Moreover, the matrix syntaxes are designed to be easy to understand for users, so users without much prior knowledge of computer science will also be able to write their own calculations using our language. 
Features like higher-order functions enable users to customize and package their calculations into functions and matrices easily and could combine and reuse these functions with more convenience. 
It also provides more readability so that users can easily understand the programmers' intentions.

Static Scoping could improve the readabilities and help users to avoid confusion about where each variable (number) is accessed from.
It also requires less run-time overhead. Strict evaluation specifies the order of executing different function arguments to avoid side-effects.
A strongly and statically typed system sets everything clear to users on what type of elements they are using and allows less confusion.
With such paradigms and features incorporated into itself, Mattricks gives the users a way to perform small to large scale matrix calculations with more efficiency and flexibility.

## Language Tutorial
**int**: 32-bit int. Explicit declaration: a = int 1;

**float**: 32-bit float. Explicit declaration: a = float 1.0;

**bool**: boolean. Explicit declaration: a = bool true;

mat: a fixed-sized, fixed dimensioned data structure representing matrices

example usage:

```cpp
a = mat int [2]; //single dimensional int matrix
a = mat float [2]; //single dimensional float matrix
a = mat bool [2]; //single dimensional bool matrix
a = mat mat int [2][2] ; //two dimensional int matrix
a = mat mat float [2][2]; //two dimensional float matrix
a = mat mat bool [2][2]; //two dimensional bool matrix
```
For example: 
```cpp
mat = int(2, 3)[[1,2,3], [5,5,5]]; 
```
Ideally, if we don’t include the bracket section in the declaration, Mattricks will simply assign the elements in matrices to 0 (for integer and float (data types).

However, in actual development, the bracket section's implementation is so complicated that we decide to leave it out for now.

**Type Casting:**

The primitive type variable cannot be statically cast to another type and type casting must be explicitly noted. 
We want to avoid implicit type casting because it could lead to unexpected results.

**Operators:**
Arithmetic: ‘+’, ‘-’. ‘*’, ‘/’, ‘%’, ‘^’ (transpose), ‘~’ (eigenvector). The first five operators’ precedence follow the same precedence as they are in C. The ‘^’ operator is the exponentiation operator. 
It has the highest precedence of all.

Due to the time constraint, we only implement the basic arithmetic operators. We considered to 
implement the matrix operators like '*' and '^' in the forms of self-defined functions, but we
decided to leave it out for now.

**Assignment:**

‘=’ is the assignment operator. It is right associative.
Declaration assignment operator does not specify the type of variables being assigned

Assignment operator does specify the type of variables being assigned. The type will need to be specified at the right-hand side of assignment.


**Equivalence:**

==, !=, >, <, >= and <= are right associative and the return of equivalence expressions is boolean or int 0 or int 1.

**Logical:**

The AND and OR operation is supported in this language.
AND has higher precedence than OR. Negation and ‘!’ operator(unary) is supported in this language. Negation is to turn some value of float and int to its negative value. 
! is used to denote logical NOT.

**Stream:** 

The stream operator is an operator used to print the data to the console. 
It is paired with the “console” and "consolef" keywords. 
The only difference between console and consolef keywords is that console prints the integer value, whereas
consolef stream prints the float value.
The right-hand side of the stream operator is the data which will be sent to the console.

```csharp
mat = int(3, 2);
console << mat;
```

**Undone features:**

Matrix Calculation:

Printing Matrix:

Function For Matrix:

## Architectural Design

![image](https://user-images.githubusercontent.com/46698520/208569434-bc44c987-e3df-49d8-bc00-009b8698994f.png)

### Scanner
The scanner.mll file is used to define all the tokens that serve the purpose of our language using lexical analysis. We built it on top of MicroC and added, modified, and removed some tokens correspondingly for our matrix-oriented language such as mat, console, consolef, gives etc. 
It raises an exception if it encounters an invalid token

### Parser
The parse.mly file takes the tokens defined in the scanner to generate an abstract syntax tree based on the grammar we created and it throws an parsing error if the input if syntactially incorrect.

### Semantic Analyzer
The semant.ml file checks if there is any semantic error by checking the AST generated by the parser and generates an SAST. Semantic checking checks if the input follows our design of grammar. 
For example, an integer cannot be declared if the assignment contains a floating point. If primarily ensures the data types for each declaration are applied correctly. 
We also use StringMap module to create a symbol table for all the variables that users create and because of the immutability of StringMap, everytime a new StringMap is created when a new variable is created. If the user tries to access a variable before its declaration, the program throws an error.

### Code Generator

The irgen.ml file uses LLVM to generate IR code based on the SAST. In the last phase of running our progam, the IR code is converted to assembly code and an executable is then created.
We also use StringMap module in ir generation to store new variables in a way similar to the process mentioned in semantic checking.

## Test Plan

We created test cases for parsing, semantic checking, and runnable tests which produce some results.
These tests include tests that should pass which are named starting with ‘pass_xxx’ and tests that should fail which are named with ‘fail_xxx’.
We created a shell script to automate the testing process. We also organized a Makefile to streamline the execution and testing of the program. The the command for running the test suite is ./test_script

**Test case 1**

```cpp
/* ./test_cases/runnable/pass_stream.mc */

function main() gives int {
  i = int 1;
  console << i;
  f = float 111.2;
  consolef << f;
  b = bool true;
  console << b;
  m = mat int [4];
  m[0] = i;
  console << m[0];
  console << 1;
  return i;
}
```

**Output:**

```cpp
1
111.200000
1
1
1
```

**IR code:**

```cpp
; ModuleID = 'MicroC'
source_filename = "MicroC"

@fmt = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1
@fmt.1 = private unnamed_addr constant [4 x i8] c"%f\0A\00", align 1

declare i32 @printf(i8*, ...)
define i32 @main() {
entry:
  %i = alloca i32, align 4
  store i32 1, i32* %i, align 4
  %i1 = load i32, i32* %i, align 4
  %printf = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt, i32 0, i32 0), i32 %i1)
  %f = alloca double, align 8
  store double 1.112000e+02, double* %f, align 8
  %f2 = load double, double* %f, align 8
  %printf3 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt.1, i32 0, i32 0), double %f2)
  %b = alloca i1, align 1
  store i1 true, i1* %b, align 1
  %b4 = load i1, i1* %b, align 1
  %printf5 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt, i32 0, i32 0), i1 %b4)
  %tmp = alloca [4 x i32], i32 0, align 4
  %i6 = load i32, i32* %i, align 4
  %tmp7 = getelementptr [4 x i32], [4 x i32]* %tmp, i32 0, i32 0
  store i32 %i6, i32* %tmp7, align 4
  %tmp8 = getelementptr [4 x i32], [4 x i32]* %tmp, i32 0, i32 0
  %tmp9 = load i32, i32* %tmp8, align 4
  %printf10 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt, i32 0, i32 0), i32 %tmp9)
  %printf11 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt, i32 0, i32 0), i32 1)
  %i12 = load i32, i32* %i, align 4
  ret i32 %i12
}

```

**Test Case 2:**

```cpp
/* ./test_cases/runnable/pass_calc_mat.mc */

/* Addition */
function main() gives int {
  mat1 = mat int [2];
  mat1[0] = 123;
  console << mat1[0];

  mat2 = mat int [2];
    mat2[0] = 321;
  console << mat2[0];

  mat3 = mat int [2];
  mat3[0] = mat1[0] + mat2[0];
  console << mat3[0];

  return 0;
}

```

**Output:**

```cpp
123
321
444
```

**IR code:**

```cpp
; ModuleID = 'MicroC'
source_filename = "MicroC"

@fmt = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1
@fmt.1 = private unnamed_addr constant [4 x i8] c"%f\0A\00", align 1

declare i32 @printf(i8*, ...)

define i32 @main() {
entry:
  %tmp = alloca [2 x i32], i32 0, align 4
  %tmp1 = getelementptr [2 x i32], [2 x i32]* %tmp, i32 0, i32 0
  store i32 123, i32* %tmp1, align 4
  %tmp2 = getelementptr [2 x i32], [2 x i32]* %tmp, i32 0, i32 0
  %tmp3 = load i32, i32* %tmp2, align 4
  %printf = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt, i32 0, i32 0), i32 %tmp3)
  %tmp4 = alloca [2 x i32], i32 0, align 4
  %tmp5 = getelementptr [2 x i32], [2 x i32]* %tmp4, i32 0, i32 0
  store i32 321, i32* %tmp5, align 4
  %tmp6 = getelementptr [2 x i32], [2 x i32]* %tmp4, i32 0, i32 0
  %tmp7 = load i32, i32* %tmp6, align 4
  %printf8 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt, i32 0, i32 0), i32 %tmp7)
  %tmp9 = alloca [2 x i32], i32 0, align 4
  %tmp10 = getelementptr [2 x i32], [2 x i32]* %tmp, i32 0, i32 0
  %tmp11 = load i32, i32* %tmp10, align 4
  %tmp12 = getelementptr [2 x i32], [2 x i32]* %tmp4, i32 0, i32 0
  %tmp13 = load i32, i32* %tmp12, align 4
  %tmp14 = add i32 %tmp11, %tmp13
  %tmp15 = getelementptr [2 x i32], [2 x i32]* %tmp9, i32 0, i32 0
  store i32 %tmp14, i32* %tmp15, align 4
  %tmp16 = getelementptr [2 x i32], [2 x i32]* %tmp9, i32 0, i32 0
  %tmp17 = load i32, i32* %tmp16, align 4
  %printf18 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt, i32 0, i32 0), i32 %tmp17)
  ret i32 0
}

```

## Summary

**Weisheng Wang (ww2609):**
- Contribution: I participated in group meetings to plan short term goals and distribute workloads. In “Hello World”, I did pair programming with Yuhao Dong on ast, parser and scanner parts; I also designed the Mat type and realized its rules. In the final phase, I did pair programming with Yuanhan Tian on redesigning the Matrix type(reimplemented the whole workflow from scanner to codegen), and I helpped with miscellaneous features.
- Takeaway: With hands on experience in developing my own programming language, I gained a deeper understanding of the lecture materials.
- Advice: Plan ahead and Set weekly and monthly milestones.

**Yuanhan Tian (yt2825):**
- Contribution:
  I reused some of the microc's example code and implemented part of the ast , the semantics checker and sast. I discussed the plans with the group members in the Zoom meetings. I collaborated with Danny Hou to implement part of the scanner.
- Takeaway:
- Advice:

**Yuhao Dong (yd2626):** 
- Contribution:
  I participated in group meetings to discuss the team plans, individual roles, and difficulties we faced. I worked with Weisheng Wang together on adding and modifying rules in ast, parser, and scanner files according to our own language features. I also worked with Weisheng Wang to fix the 2d matrix parsing rules to make it work as designed and to eliminate the conflicts.- Takeaway:
- Advice:

**Yuxin Yang (yy3277):** 
- Contribution:
1. Created test cases for testing the parser of the program matching with the features mentioned in LRM; 2. Organized and managed this repository and group meetings 3. Helped identify some issues and existing bugs in the grammar; 4. Updated LRM- Takeaway:
- Advice:

**Hang Yuan (hy2784):** 
- Contribution:
1. Helped define the part of the proposal and LRM, proofread and finalize it; 2. Designed and implemented the test automation script; 3. Wrote all test cases for semantics phase; 4. Found out grammar ambiguity, created Github Issues for sytax/semantics bug; 5. Helped debug a small portion of the scanner and parser.- Takeaway:
- Advice:

**Danny Hou (dh3034):** 
- Contribution:
  I discussed our plan with team memebrs during group meetings and collaborated with Yuanhan to work on the tokens and rules of our scanner reusing some of the microc's example.- Takeaway:
- Advice:

**Jack Wang (yw4014):** 
- Contribution:
  I parcipated in dicussions on implementation, ast, and semant. The discussions finalized key items for submission.- Takeaway:
- Advice: