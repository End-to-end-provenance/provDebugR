For each line number queried, `debug.line` returns a data frame of the data 
that the procedure in that line inputs and outputs.

Each data frame contains the following columns:
* name: The name of the data.
* value: The value of the data.
* container: The type of the container of the data.
* dimension: The size of the container.
* type: The data type(s) contained within the container.


## Usage

The function signature for `debug.line` is:
```
debug.line(..., script.num = 1, all = FALSE)
```

The parameters of this function are:
* `...` The line numbers to be queried
* `script.num` The script number of the queried line numbers. 
Allows for only 1 script number to be queried per function call. 
Defaults to script number 1 (main script).
* `all` If TRUE, the inputs and outputs for all lines in the specified 
script will be returned.

In the case of multiple queries, only 1 script number may be queried per
function call.

This function may be called only after initialising the debugger using either 
`prov.debug`, `prov.debug.run`, or `prov.debug.file`. For example:
```
prov.debug.run("myScript.R")
debug.line(5)
debug.line(all = TRUE)
debug.line(5, 10, script.num = 2)
```


## Output Examples
Let `myScript.R` be the following:
```
x <- 1:3
y <- c("a", "b", "c")
xy <- data.frame(x, y, stringsAsFactors = FALSE)
x
```

### 1. The line has both inputs and output data nodes
The result for `debug.line(3)` is:
```
Results for:
  startLine scriptNum                                             code
1         3         1 xy <- data.frame(x, y, stringsAsFactors = FALSE)

$`1`
$`1`$input
  name       value container dimension      type
1    x       1 2 3    vector         3   integer
2    y "a" "b" "c"    vector         3 character

$`1`$output
  name     value  container dimension               type
1   xy Row 1 1 a data_frame       3,2 integer, character
```
Variables `x` and `y` are listed in the 'input' table as they are used in the procedure.

Variable `xy` is listed in the 'output' as the result from the 
procedure is assigned to that variable.

### 2. The line has either inputs or outputs
If the line has only inputs or outputs, an `NA` will be put in the place of the
data frame with no data nodes listed.

Line 1 has no inputs, so the result for `debug.line(1)` is:
```
Results for:
  startLine scriptNum     code
1         1         1 x <- 1:3

$`1`
$`1`$input
[1] NA

$`1`$output
  name value container dimension    type
1    x 1 2 3    vector         3 integer
```

Line 4 has no outputs, so the result for `debug.line(4)` is:
```
Results for:
  startLine scriptNum code
1         4         1    x

$`1`
$`1`$input
  name value container dimension    type
1    x 1 2 3    vector         3 integer

$`1`$output
[1] NA
```

### 3. Results for multiple line queries
If multiple lines are queried, the results for each line will be bound into a list.
The resulting data structure is a list of lists of 2 data frames.

For example, the result for `debug.line(1,3,4)` is:
```
Results for:
  startLine scriptNum                                             code
1         1         1                                         x <- 1:3
2         3         1 xy <- data.frame(x, y, stringsAsFactors = FALSE)
3         4         1                                                x

$`1`
$`1`$input
[1] NA

$`1`$output
  name value container dimension    type
1    x 1 2 3    vector         3 integer


$`2`
$`2`$input
  name       value container dimension      type
1    x       1 2 3    vector         3   integer
2    y "a" "b" "c"    vector         3 character

$`2`$output
  name     value  container dimension               type
1   xy Row 1 1 a data_frame       3,2 integer, character


$`3`
$`3`$input
  name value container dimension    type
1    x 1 2 3    vector         3 integer

$`3`$output
[1] NA
```