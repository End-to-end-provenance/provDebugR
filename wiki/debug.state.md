For each queried line, `debug.state` returns a data frame showing the state at that 
line, after it has been executed.

Each data frame contains the following columns:
* name: The names of variables in the state.
* value: The value of each variable.
* container: The type of the container of each variable.
* dimension: The size of the container.
* type: The data type(s) contained within the container.
* scriptNum: The script number associated with each variable.
* startLine: The line number associated with each variable.

If no paramters are given, `debug.state` will return the state at the end of execution.


## Usage
The function signature for `debug.state` is:
```
debug.state(..., script.num = 1)
```

The parameters of this function are:
* `...` The line numbers to be queried.
* `script.num` The script number of the queried line numbers. 
This is ignored if no line numbers are given.

Only 1 script number may be queried per function call.

This function may be called only after initialising the debugger using either 
`prov.debug`, `prov.debug.run`, or `prov.debug.file`. For example:
```
prov.debug.run("myScript.R")
debug.state()
debug.state(5)
debug.state(10, 20, script.num = 2)
```


## Output Examples

Let `myScript.R` be the following:
```
a <- "one"
a <- 2L

b <- 3L
b <- "four"
```

### 1. State at the end of execution
When there are no parameters, this function returns the state at the end of
execution.

Therefore, the result for `debug.state()` is:
```
State at the end of execution:
$`1`
  name  value container dimension      type scriptNum startLine
1    a      2    vector         1   integer         1         2
2    b "four"    vector         1 character         1         5
```

### 2. State at a line
This function returns the state at the queried line, after the line has been
executed.

For example, the result for `debug.state(1)` is:
```
Results for:
  startLine scriptNum
1         1         1

$`1`
  name value container dimension      type scriptNum startLine
1    a "one"    vector         1 character         1         1
```

Similarly, the result for `debug.state(4)` is:
```
Results for:
  startLine scriptNum
1         4         1

$`1`
  name value container dimension    type scriptNum startLine
1    a     2    vector         1 integer         1         2
2    b     3    vector         1 integer         1         4
```

### 3. State at multiple lines
When multiple lines are queried in a function call, the results for each line
are bound into a list.

For example, the result for `debug.state(2,4)` is:
```
Results for:
  startLine scriptNum
1         2         1
2         4         1

$`1`
  name value container dimension    type scriptNum startLine
1    a     2    vector         1 integer         1         2

$`2`
  name value container dimension    type scriptNum startLine
1    a     2    vector         1 integer         1         2
2    b     3    vector         1 integer         1         4
```