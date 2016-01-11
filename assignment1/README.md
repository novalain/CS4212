To process this directory please use the make utility to create the executable if it does not exist in the folder (e.g run the command "make" in your bash/terminal when you are in the same folder as the Makefile). 

All the testcases are divided into groups, their location are TESTCASES/{{subtask}}/{{name_of_the_txt_file}}. An example to run the file "basic_comments.txt" from task1 is provided below:

##### WINDOWS: 
```
make
mOOL_parser TESTCASES/task1/basic_comments.txt
```

##### OSX:
```
make
./mOOL_parser TESTCASES/task1/basic_comments.txt
```

Description of the following assignments:

##Task1

The implementation of the comments was done in mOOL_lexer.mll between lines 89 and 103. The comments make their own buffer which calls itself recursively.
When the lexer discovers single comments somewhere in the code "//", it simply continious to read all characters until it discovers the newline character. 

For the multi-line-comments a variable n is passed that keeps track of the current level of the comments through recursive calls. This means that whenever the lexer discovers an additional "/*" character, it increases the counter. Additionally, when one level of comments ends with the character "*/", the counter goes down again. If the comments are never closed, an error message occurs, if the comments are closed and level is 0, it is done and "ok".

String literals was defined in mOOL_lexer.mll. All of the string literals start and ends with a doublequote. When can be inside the doublequotes is defined by an escaped character followed by anything, OR all of the characters that are not an escape character or double quote.

##Task 2

The expressions and grammar for the mOOL languange were implemented as stated in the exercise, please see the test programs for examples of the expressions. The code has at the moment 19 shift/reduce conflicts, 6 reduce/reduce conflicts and 1 rule that is never reduced which is sexp : atom (maybe that rule could be removed since both SExp and BGrd are both pointing to an Atom, but I decided to follow the grammar given by professor in PDF). 

Ocamlyacc is designed to shift rather than reduce and the conflicts should thus be fine in this case. Writing unambigous grammar would of course be ideal, I've read that it is possible to give the grammar priority rules and explicitly tell what rules should be fired when there is ambiguity, but I didn't know how to implement that.

##Task 3

Class components were simply implemented by defining new keywords in the lexer and make the parser accept the necessary grammar. When a class is extending another class the second parameter which is of type option, was changed to the name of the parent class. 

##Task 4

For the parser to be able to accept zero or more attributes followed by zero or more methods, two different lists was first implemented: one that held all the attributes and another list that held all method. That turned out not to work because the parser did not know when to stop looking for variables. In the second attempt all the variables and only the first method were stored in one datastructure; an array, this did not work out either due to some syntax problems or lack of OCaml skills (please see lines 189-200). The third and final successfull attempt was to store each list of attributes and methods in a pair. The list of the variables was made left recursive so that it could stop right after a single method had been found. Another list was then declared to keep track of the rest 2* methods in the class.
