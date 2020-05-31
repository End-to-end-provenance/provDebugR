For each variable queried, `debug.variable` returns a data frame of all 
instances (data nodes) of that variable.

Each data frame contains the following columns:
* `value` The value of the variable.
* `container` The type of the container of the variable.
* `dimension` The size of the container.
* `type` The data type(s) contained within the container.
* `scriptNum` The script number the variable is associated with.
* `scriptName` The name of the script the variable or file is associated with.
* `startLine` The line number the variable is associated with.
* `code` The code this variable is associated with.

## Usage

The function signature for `debug.variable` is:
```
debug.variable(..., val.type = "all", script.num = "all", all = FALSE)
```

The parameters of this function are:
* `...` The variable names to be queried.
* `val.type` If not "all", this filters the results to contain 
only instances where the valType (container or type) has the queried type.
* `script.num` The script number of the queried variables. Defaults to "all".
* `all` If TRUE, results for all variables of the specified script will be returned.

Only 1 script number and/or valType may be queried per function call.

This function may be called only after initialising the debugger using either 
`prov.debug`, `prov.debug.run`, or `prov.debug.file`. For example:
```
prov.debug.run("myScript.R")
debug.variable(x)
debug.variable(all = TRUE)
debug.variable("a", b, "x", val.type = "logical")
debug.variable("a", "b", x, script.num = 3)
```


## Output Examples

Let `myScript.R` be the following:
```
a <- "one"
a <- "two"
a <- 1L
a <- 2L

b <- 3L
b <- 4L
b <- "five"
```

### 1. Checking for all instances of a variable
The result for `debug.variable("a")` is:
```
$a
  value container dimension      type scriptNum scriptName startLine       code
1 "one"    vector         1 character         1 myScript.R         1 a <- "one"
2 "two"    vector         1 character         1 myScript.R         2 a <- "two"
3     1    vector         1   integer         1 myScript.R         3    a <- 1L
4     2    vector         1   integer         1 myScript.R         4    a <- 2L
```

### 2. Checking for all instances of a variable with a certain type
The result for `debug.variable("a", val.type = "integer")` is:
```
$a
  value container dimension    type scriptNum scriptName startLine    code
1     1    vector         1 integer         1 myScript.R         3 a <- 1L
2     2    vector         1 integer         1 myScript.R         4 a <- 2L
```

### 3. Checking for all instances of multiple variables
When multiple variables are queried, the results for each variable are
bound together in a list, resulting a list of data frames.

The result for `debug.variable("a","b")` is:
```
$a
  value container dimension      type scriptNum scriptName startLine       code
1 "one"    vector         1 character         1 myScript.R         1 a <- "one"
2 "two"    vector         1 character         1 myScript.R         2 a <- "two"
3     1    vector         1   integer         1 myScript.R         3    a <- 1L
4     2    vector         1   integer         1 myScript.R         4    a <- 2L

$b
   value container dimension      type scriptNum scriptName startLine        code
1      3    vector         1   integer         1 myScript.R         6     b <- 3L
2      4    vector         1   integer         1 myScript.R         7     b <- 4L
3 "five"    vector         1 character         1 myScript.R         8 b <- "five"
```

### 4. Checking for all instances of a multiple variables with a certain type
The result for `debug.variable("a", "b", val.type = "integer")` is:
```
$a
  value container dimension    type scriptNum scriptName startLine    code
1     1    vector         1 integer         1 myScript.R         3 a <- 1L
2     2    vector         1 integer         1 myScript.R         4 a <- 2L

$b
  value container dimension    type scriptNum scriptName startLine    code
1     3    vector         1 integer         1 myScript.R         6 b <- 3L
2     4    vector         1 integer         1 myScript.R         7 b <- 4L
```