# provDebugR

A debugging tool that leverages provenance to provide handy information about R scripts to assist in writing them.

# Installation
Devtools is needed for installation:
```{r}
install.packages("devtools")
```
Installation with dependencies (copy and paste):
```{r}
install.packages(igraph)
devtools::install_github("ProvTools/provParseR")
devtools::install_github("jwons/provGraphR")
devtools::install_github("End-to-end-provenance/RDataTracker", ref = "development")
devtools::install_github("jwons/provDebugR")
```
Once installed, load the package:
```{r}
library("provDebugR")
```

# Usage
While writing a script, run the script by calling:
```{r}
debug.init("yourScriptNameHere.R")
```
If you have provenance stored as prov.json somewhere you can alternatively run:
```{r}
debug.prov("yourProvJsonHere.json")
```

Once either debug.prov or debug.init is run the rest of the functions can be used. 

```{r}
debug.init(input.data)
debug.prov(input.prov, is.file = F)
debug.variable.type(..., just.logical = F)
debug.from.type(variable, type)
debug.from.line(..., state = F) 
debug.lineage(..., forward = F) 
debug.error.trace(stack.overflow = F)
debug.warning.trace(..., stack.overflow = F) 
```
