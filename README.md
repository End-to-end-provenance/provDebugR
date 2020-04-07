# provDebugR

A debugging tool that leverages provenance to provide handy information about R scripts to assist in writing them.

For a more in-depth description of the functions available in this package [refer to the Wiki.](https://github.com/ProvTools/provDebugR/wiki)

# Installation

Version 3.5.0 or later of R is required.

Devtools is needed for installation:
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

Once installed, load provDebugR by calling:
```{r}
library("provDebugR")
```

# Usage

To initialise the debugger with a script, call:
```{r}
prov.debug.run("scriptName.R")
```
Alternatively, if you just called [`rdtLite`](https://cran.r-project.org/web/packages/rdtLite/index.html)'s
`prov.run` function, you can call:
```{r}
prov.debug()
```
Lastly, if you have the PROV-JSON provenance file, you can also call:
```{r}
prov.debug.file("provJsonFileName.json")
```

Once the debugger has been initialised, the rest of the functions the package
provides can be used.
To find out more about what each function does, [refer to the Wiki!](https://github.com/ProvTools/provDebugR/wiki)
* debug.error
* debug.line
* debug.lineage
* debug.state
* debug.type.changes
* debug.variable
* debug.warning
