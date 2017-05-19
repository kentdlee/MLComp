MLComp
======

A Compiler and Type Inference System for a subset of Standard ML called Small. 

**********************************************************
* MLComp is a compiler for the Small language. Small is  
* a subset of Standard ML with a few additional functions
* that may be called. This compiler is written in        
* Standard ML and targets the CoCo Virtual Machine. The
* code provided here is a partial implementation of the 
* compiler, serving as a starting point for students
* working in the text "Fundamentals of Programming 
* Languages" by Kent D. Lee and published by Springer. 
* This code is free for educational use. Any derivation
* of this code for commercial purposes must be licensed
* by Kent Lee. 
* All code in this project,
* (c) 2014, 2017 by Kent D. Lee, All Rights Reserved. 
*********************************************************

The project requires Standard ML of New Jersey be installed, 
SWI Prolog, and the CoCo or JCoCo virtual machine. Once these 
three platforms are installed the project can be compiled and
run with these commands.

% make
% mlcomp test0.sml

The project consists of both a compiler and a type checker. The
compiler is written in Standard ML and targets the CoCo virtual
machine. The type checker is written in Prolog and implements the 
Standard ML type inference system. The call to the type checker 
is initially commented out in the mlcomp script. When beginning
to work with the type checker uncomment the two lines that call 
the type checker in the mlcomp compiler script.

Additional tests are found in this directory. Some will work 
right away, others require the compiler be extended to support
them. 

Any questions can be directed to kentdlee@luther.edu.

