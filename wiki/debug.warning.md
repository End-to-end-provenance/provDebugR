`debug.warning` returns a data frame representing the backwards lineage of
(the steps leading up to) each warning queried.

Each data frame contains the following columns:
* scriptNum: The script number the data node is associated with.
* startLine: The line number the data node is associated with.
* code: The line of code which used/produced the data node.

### Usage

The function signature for `debug.warning` is:
```
debug.warning(..., all = FALSE)
```

The parameters for this function are:
* `...` The index of the warning(s) to be queried. As all warning nodes have the 
same name, their index (order of occurrence) is used to distinguish them.
* `all` If TRUE, the backwards lineages of all warnings are returned.

This function may be called only after initialising the debugger using either 
`prov.debug`, `prov.debug.run`, or `prov.debug.file`. For example:
```
prov.debug.run("myScript.R")
debug.warning(1)
debug.warning(2,3)
debug.warning(all = TRUE)
```


### Output Examples

Let `myScript.R` be the following:
```
warning("This is a test")

x <- 1
y <- 2
z <- 3
cor(c(x, x), c(y, z))
```
Running this script will result in 2 warnings, the first from line 1, and the
second from line 6:
```
Warning messages:
1: In eval(annot, environ, NULL) : This is a test
2: In cor(c(x, x), c(y, z)) : the standard deviation is zero
```

### 1. Obtaining the lineage for a warning
Calling `debug.warning(2)` will give the backwards lineage of the second warning:
```
$`2`
  scriptNum startLine                  code
1         1         3                x <- 1
2         1         4                y <- 2
3         1         5                z <- 3
4         1         6 cor(c(x, x), c(y, z))
```

### 2. Obtaining the lineage for multiple warnings
Multiple warnings may be queried at once to get the lineages of multiple warnings.

For example, the result for `debug.warning(1,2)` is:
```
$`1`
  scriptNum startLine                      code
1         1         1 warning("This is a test")

$`2`
  scriptNum startLine                  code
1         1         3                x <- 1
2         1         4                y <- 2
3         1         5                z <- 3
4         1         6 cor(c(x, x), c(y, z))
```

### 3. No parameters
If no parameters are passed to `debug.warning`, a table of all warnings, if any,
will be displayed.

For example, calling `debug.warning()` will result in:
```
Possible options:
                                                             
1            In  eval(annot, environ, NULL) :  This is a test
2 In  cor(c(x, x), c(y, z)) :  the standard deviation is zero

Pass the corresponding numeric value to the function for info on that warning.
```