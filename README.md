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
devtools::install_github("ProvTools/provParseR")
devtools::install_github("jwons/provGraphR")
devtools::install_github("End-to-end-provenance/RDataTracker", ref = "development")
devtools::install_github("ProvTools/provDebugR")
```
This package also imports:
* jsonlite
* igraph 
* httr 
* methods
* stats
* testthat

and if your environment is RStudio:
* rstudioapi
* shiny
* miniUI


Once installed, load the package:
```{r}
library("provDebugR")
```

# Usage
While writing a script, run the script by calling:
```{r}
debug.init("yourScriptNameHere.R")
```
If you already have provenance stored as prov.json, you can also 
use that file as an argument.
```{r}
debug.init("prov.json")
```

Once either debug.prov or debug.init is run the rest of the functions can be used. 
To find out more about what each function does, [refer to the Wiki!](https://github.com/ProvTools/provDebugR/wiki)
```{r}
debug.init(input.data)
debug.variable.type(..., just.logical = F)
debug.from.type(variable, type) -- ***GOING TO BE REMOVED***
debug.from.line(..., state = F) 
debug.lineage(..., forward = F) 
debug.error.trace(stack.overflow = F)
debug.warning.trace(...) 
debug.browser()
debug.gadget()
```
