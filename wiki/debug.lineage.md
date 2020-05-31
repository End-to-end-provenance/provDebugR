For each data node queried, `debug.lineage` returns a data frame representing its 
forwards (how the data is used), or backwards (how the data was generated) lineage. 

Each data frame contains the following columns:
* `scriptNum` The script number the data node is associated with.
* `scriptName` The name of the script the data node is associated with.
* `startLine` The line number the data node is associated with.
* `code` The line of code which used/produced the data node.


## Usage

The function signature for `debug.lineage` is:
```
debug.lineage(..., start.line = NA, script.num = 1, all = FALSE, forward = FALSE)
```

The parameters of this function are:
* `...` The names of data nodes to be queried.
* `start.line` The line number of the queried data nodes. Optional.
* `script.num` The script number of the queried data nodes. 
Defaults to script number 1 (main script).
* `all`	If TRUE, this function returns the linages of all data node names.
`forward` If TRUE, this function returns the forwards lineage (how the data is used) 
instead of the backwards lineage (how the data was generated).

This function may be called only after initialising the debugger using either 
`prov.debug`, `prov.debug.run`, or `prov.debug.file`. For example:
```
prov.debug.run("myScript.R")
debug.lineage(x)
debug.lineage("x", start.line = 5, script.num = 2)
debug.lineage("a", b, forward = TRUE)
debug.lineage(all = TRUE)
```


## Output Examples

Let `myScript.R` be the following:
```
a <- 1
b <- 2
cc <- 4

v1 <- c(a:10)
v1 <- rep(v1, b)

m1 <- matrix(v1, cc)
print(m1)
```

### 1. Backwards lineage of a variable
By default, the backwards lineage of the queried object is returned when the
function is called, showing how it was derived.

If no start lines are specified, the backwards lineage of the last occurence of that
object will be returned.

For example, the result for `debug.lineage(v1)` is:
```
$v1
  scriptNum scriptName startLine             code
1         1 myScript.R         1           a <- 1
2         1 myScript.R         2           b <- 2
3         1 myScript.R         5    v1 <- c(a:10)
4         1 myScript.R         6 v1 <- rep(v1, b)
```
The backwards lineage for the `v1` variable at line 6 is given as it is the last 
occurence of that variable.

The `start.line` parameter is used to specify which occurence of the queried
variable the lineage should be returned for.

For example, `debug.lineage("v1", start.line = 5)` will return the backwards
lineage of `v1` at line 5, resulting in:
```
$v1
  scriptNum scriptName startLine          code
1         1 myScript.R         1        a <- 1
2         1 myScript.R         5 v1 <- c(a:10)
```

### 2. Forwards lineage of a variable
If the `forward` parameter is set to `TRUE`, the forwards lineage will be returned
instead of the default backwards lineage, showing how that object was used.

If no start lines are specified, the forwards lineage of the first occurence of that
object will be returned.

For example, the result for `debug.lineage("v1", forward = TRUE)` is:
```
$v1
  scriptNum scriptName startLine                 code
1         1 myScript.R         5        v1 <- c(a:10)
2         1 myScript.R         6     v1 <- rep(v1, b)
3         1 myScript.R         8 m1 <- matrix(v1, cc)
4         1 myScript.R         9            print(m1)
```
The forwards lineage for the `v1` variable at line 5 is given as it is the first
occurrence of that variable.

The `start.line` parameter may also be used to specify which occurence of the queried
variable the forwards lineage should be returned for.

For example, `debug.lineage("v1", start.line = 6, forward = TRUE)` will
return the forwards lineage of the `v1` variable at line 6, resulting in:
```
$v1
  scriptNum scriptName startLine                 code
1         1 myScript.R         6     v1 <- rep(v1, b)
2         1 myScript.R         8 m1 <- matrix(v1, cc)
3         1 myScript.R         9            print(m1)
```

### 3. Lineage queries for multiple objects
Multiple objects may be queried per function call for both backwards and forwards
lineages, but only 1 start line may be specified in this case.

For example, the result for `debug.lineage("a", "v1")` is:
```
$a
  scriptNum scriptName startLine   code
1         1 myScript.R         1 a <- 1

$v1
  scriptNum scriptName startLine             code
1         1 myScript.R         1           a <- 1
2         1 myScript.R         2           b <- 2
3         1 myScript.R         5    v1 <- c(a:10)
4         1 myScript.R         6 v1 <- rep(v1, b)
```

The result for `debug.lineage("b", "m1", forward = TRUE)` is:
```
$b
  scriptNum scriptName startLine                 code
1         1 myScript.R         2               b <- 2
2         1 myScript.R         6     v1 <- rep(v1, b)
3         1 myScript.R         8 m1 <- matrix(v1, cc)
4         1 myScript.R         9            print(m1)

$m1
  scriptNum scriptName startLine                 code
1         1 myScript.R         8 m1 <- matrix(v1, cc)
2         1 myScript.R         9            print(m1)
```