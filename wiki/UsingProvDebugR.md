# Installation

Version 3.5.0 or later of R is required.

devtools is needed for installation:
```{r}
install.packages("devtools")
```
Installation of all required packages (can be copied and pasted):
```{r}
install.packages("provParseR")
install.packages("provGraphR")
install.packages("rdtLite")
devtools::install_github("End-to-end-provenance/provDebugR")
```
provDebugR also imports:
* httr
* igraph 
* jsonlite
* methods
* testthat
* textutils


# Initialisation

provDebugR must be initialised using one of the following functions before the rest
of its suite of functions becomes available.
* prov.debug.run
* prov.debug
* prov.debug.file

### `prov.debug.run(script)`
`prov.debug.run` takes in the path to an R or Rmd script and executes the script 
using [rdtLite](https://cran.r-project.org/web/packages/rdtLite/index.html) to 
collect provenance before initialising the debugger.
```
library(provDebugR)
prov.debug.run("myScript.R", snapshot.size = 100)
```

### `prov.debug()`
Alternatively, if 
[rdtLite](https://cran.r-project.org/web/packages/rdtLite/index.html)'s 
`prov.run` function was just called, the provenence stored in memory can be used
directly to initialise the debugger.
```
library(rdtLite)
library(provDebugR)
prov.run("myScript.R", snapshot.size = 100)
prov.debug()
```

### `prov.debug.file`
Lastly, the debugger may also be initialised using a PROV-JSON provenance file.
```
library(rdtLite)
prov.debug.file("provJsonFileName.json")
```


# Other provDebugR Functions

After the debugger has been initialised, the rest of provDebugR's suite of functions
may be used.

### [`debug.error`](https://github.com/End-to-end-provenance/provDebugR/wiki/debug.error)
This functions returns the backwards lineage of (steps leading up to) the error, if any.
The error may then be searched for on Stack Overflow.

### [`debug.line`](https://github.com/End-to-end-provenance/provDebugR/wiki/debug.line)
This function enables the user to see what the queried line(s) immediate inputs and
outputs are, if any.

### [`debug.lineage`](https://github.com/End-to-end-provenance/provDebugR/wiki/debug.lineage)
This function returns the forwards or backwards lineage of the data object(s) queried. 
The forwards lineage shows how the data object was used, and the backwards lineage shows 
how it was produced.

### [`debug.state`](https://github.com/End-to-end-provenance/provDebugR/wiki/debug.state)
This function returns the state at the line(s) queried, after the line had been 
executed. The state is the list of all variables and their values in the environment 
at the queried line.

### [`debug.type.changes`](https://github.com/End-to-end-provenance/provDebugR/wiki/debug.type.changes)
This function returns a data frame for each variable in the execution containing the 
instances where the data type changed.

### [`debug.variable`](https://github.com/End-to-end-provenance/provDebugR/wiki/debug.variable)
For each variable queried, this function returns a data frame showing all instances of
the queried variable.

### [`debug.warning`](https://github.com/End-to-end-provenance/provDebugR/wiki/debug.warning)
This function returns the backwards lineage of the queried warning(s), if any.