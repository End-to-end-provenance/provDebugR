`debug.error` returns a data frame representing the backwards lineage of 
(the steps leading up to) the error in the execution, if any.

Each data frame contains the following columns:
* scriptNum: The script number the data node is associated with.
* startLine: The line number the data node is associated with.
* code: The line of code which used/produced the data node.


### Usage

The function signature for `debug.error` is:
```
debug.error(stack.overflow = FALSE)
```

The parameter for this function is:
* `stack.overflow` If TRUE, the error message will be searched for on Stack Overflow.

This function may be called only after initialising the debugger using either 
`prov.debug`, `prov.debug.run`, or `prov.debug.file`. For example:
```
prov.debug.run("myScript.R")
debug.error()
debug.error(stack.overflow = TRUE)
```


### Usage Examples

Let `myScript.R` be the following:
```
x <- 1
x <- a + x
```

Running this script will result in the following error:
```
Error in eval(annot, environ, NULL) : object 'a' not found
```

### 1. `stack.overflow` is FALSE
The backwards lineage of the error in the script, if any, is returned.

The result of `debug.error()` is:
```
Your Error: Error in eval(annot, environ, NULL): object 'a' not found

Code that led to error message:

  scriptNum startLine       code
1         1         1     x <- 1
2         1         2 x <- a + x
```

### 2. `stack.overflow` is TRUE
In addition to returning the backwards lineage of the error, the error will also
be searched on Stack Overflow.

The result of `debug.error(stack.overflow = TRUE)` is:
```
Your Error: Error in eval(annot, environ, NULL): object 'a' not found

Code that led to error message:

  scriptNum startLine       code
1         1         1     x <- 1
2         1         2 x <- a + x


Results from StackOverflow:
[1] "Object not found error with ddply inside a function"                  
[2] "ggplot object not found error when adding layer with different data"  
[3] "Error in eval(expr, envir, enclos) : object not found"                
[4] "data.table throws \"object not found\" error"                         
[5] "Object not found error when passing model formula to another function"
[6] "Object not found error with ggplot2"                                  

Choose a numeric value that matches your error the best or q to quit: 
```
An integer between 1 and 6 (inclusive) can be entered, or 'q' to exit the function.
If a number between 1 and 6 is chosen, a window will pop up linking to the page on
Stack Overflow. 

The function will continue to wait for input (not terminate) until 'q' has been given.

Once 'q' has been given, the function will return with the error's backwards lineage.
```
Code that led to error message:

  scriptNum startLine       code
1         1         1     x <- 1
2         1         2 x <- a + x
```