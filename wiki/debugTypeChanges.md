Returns a data frame for each variable in the execution containing the instances 
where the data type changed. 

Each data frame contains the following columns:
* value: The value of the variable.
* container: The type of the container of the variable.
* dimension: The size of the container.
* type: The data type(s) contained within the container.
* code: The line of code associated with the variable.
* scriptNum: The script number associated with the variable.
* startLine: The line number associated with the variable.


## Usage

The function signature for `debug.type.changes` is:
```
debug.type.changes(var = NA)
```

The parameter of this function is:
* `var` Optional. Variable name(s) to be queried. If not NA, the results 
will be filtered to show only those with the given variable name.

This function may be called only after initialising the debugger using either 
`prov.debug`, `prov.debug.run`, or `prov.debug.file`. For example:
```
prov.debug.run("myScript.R")
debug.type.changes()
debug.type.changes(var = "x")
debug.type.changes(var = c("a", "b"))
```

## Output Examples

Let `myScript.R` be the following:
```
a <- "one"
a <- "two"
a <- 1L
a <- 2L

b <- 3L
b <- "three"

cc <- c(1:3)
cc <- TRUE
```

### 1. Checking for type changes in all variables
The result for `debug.type.changes()` is:
```
$a
  value container dimension      type       code scriptNum startLine
1 "two"    vector         1 character a <- "two"         1         2
2     1    vector         1   integer    a <- 1L         1         3

$b
    value container dimension      type         code scriptNum startLine
1       3    vector         1   integer      b <- 3L         1         6
2 "three"    vector         1 character b <- "three"         1         7

$cc
  value container dimension    type         code scriptNum startLine
1 1 2 3    vector         3 integer cc <- c(1:3)         1         9
2  TRUE    vector         1 logical   cc <- TRUE         1        10

```
Lines 1 and 4 are omitted as there are no type changes to variable `a` 
to and from those lines.

### 2. Checking for type changes in certain variables
By using the parameter `var`, the results may be filtered to be from
only the queried variables.

For example, the result for `debug.variable(var = "a")` is:
```
$a
  value container dimension      type       code scriptNum startLine
1 "two"    vector         1 character a <- "two"         1         2
2     1    vector         1   integer    a <- 1L         1         3
```

Similarly, the result for `debug.variable(var = c("a", "cc"))` is:
```
$a
  value container dimension      type       code scriptNum startLine
1 "two"    vector         1 character a <- "two"         1         2
2     1    vector         1   integer    a <- 1L         1         3

$cc
  value container dimension    type         code scriptNum startLine
1 1 2 3    vector         3 integer cc <- c(1:3)         1         9
2  TRUE    vector         1 logical   cc <- TRUE         1        10
```