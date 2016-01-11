To process this directory please use the make utility to create the executable if it does not exist in the folder (e.g run the command "make" in your bash/terminal when you are in the same folder as the Makefile e.g partA or partB). 

* Custom testcases are found in the folder TESTCASES/custom/{{name_of_test_program}
* Sample testcases are found in the folder TESTCASES/{{name_of_sample_testcase}

An example to run the custom program "basic_comments.mo" from task1 is provided below:

WINDOWS: 

###PartA:
```
make
mOOL_checker TESTCASES/custom/basic_inheritance.mo
```

###PartB:
```
make
mOOL_ir3_Gen TESTCASES/custom/basic_inheritance.mo
```

OSX:

###PartA:
```
make
./mOOL_checker TESTCASES/custom/basic_inheritance.mo
```

###PartB:
```
make
./mOOL_ir3_Gen TESTCASES/custom/basic_inheritance.mo
```

======================================================================

Description of the following assignments:

## PART A

###Static-checking
Each seperate subtask (1-7) is performed by a single function in the first function type_check_mOOL_program, here we check for unique class names, valid class inheritance, distinct class attributes, distinct parameters in method, distinct local variables in method and finally such that only overloading and overriding metods are valid.

The static checking could probably be done in the same functions as the type-checking and it would have been more efficient, but lack of skills in OCaml made it easier to begin to check every single part separately.

For details please see lines 680 - 685 where each subtask is represented by a single function.

###Type-checking
The type-checking is done recursively with the help of the type-checking-rules through the parse tree, where the main mOOL program is broken down into smaller and smaller parts, where the recursion stops at the expressions. For every statement the type is checked such that it correspons to the expected type. 

###Methods 
When a method is called through fieldaccess on an object, the type-checker goes to look for the same method with the same signature (parameters) in the type of the object that was called (callee) The static checker makes sure that the methods cannot have the same signature and return type. Method overloading is allowed (The callee is going to look for the method whose signature that match). For an example please see lines 66-77 in custom test-case method_overloading.

###Structure
The class declaration list is passed to each function through the whole program to be able to iterate through the mOOL program when needed to find variables or methods in other classes than the "current" class. 

##PART B

* In part B the typed parse tree is converted to the language IR3 that respresents three-adress-code
* For every variable we assume it is a class attribute. 
* For the methods we add an unique identifier to the mOOL method field ir3id. The method name is then mapped to methodname with its corresponding class name and the unique identifier. This makes sure that each method is unique even though they have the same method name.
