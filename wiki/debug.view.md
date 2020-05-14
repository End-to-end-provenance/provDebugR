`debug.view` displays the contents of each file or variable queried.
For snapshots or files with the file extension of .csv or .txt, the data
will be loaded into the debugger environment before it is viewed. Otherwise,
the data will be viewed using the system's default program for that type of file.

Additionally, a data frame showing what the function has opened will be returned,
which contains the following columns:
* name: The name of the variable or file being viewed.
* startLine: The line number the variable or file is associated with. 
* scriptNum: The script number the variable or file is associated with.
* title: The title of the variable or file when viewed.
*  notes: Will display PARTIAL if the variable is a partial snapshot, or
		  indicate that the provenance directory or a file is not found.
		  NA otherwise.

## Usage

The function signature for `debug.view` is:
```
debug.view(..., start.line = NA, script.num = 1)
```

The parameters of this function are:
* `...` The variable names or file names to be queried.
* `start.line` The line number of the queried variables or files. Optional.
* `script.num` The script number of the queried variables or files. Defaults to 1.

Only 1 script number may be queried per function call.Multiple start lines
may be queried if and only if 1 object name is queried.

This function may be called only after initialising the debugger using either 
`prov.debug`, `prov.debug.run`, or `prov.debug.file`. For example:
```
prov.debug.run("myScript.R")
debug.view("x")
debug.view("x", "y", start.line = 5, script.num = 2)
```

