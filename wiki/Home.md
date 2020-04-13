# Welcome to the provDebugR wiki!

Welcome! This wiki is designed to help users make use of and better understand
provDebugR.

## What is provDebugR?
provDebugR is a time-travelling debugging tool that leverages provenance and is
created for use on R scripts.

provDebugR uses provenance produced by 
[rdtLite](https://cran.r-project.org/web/packages/rdtLite/index.html),
a provenance collection library. This provenance is stored in 
[PROV-JSON format](https://github.com/End-to-end-provenance/ExtendedProvJson/blob/master/JSON-format.md),
which follows the 
[W3 PROV-JSON standard](https://www.w3.org/Submission/2013/SUBM-prov-json-20130424/).

## What is provenance?
Provenance is a detailed record of an execution of a script. This includes 
information about the steps that were excecuted and the intermediate data values 
that were used and/or created.

provDebugR was created to promote the use of provenance in scientific analysis, to
make data analysis more reproducible and transparent. 
[Click here](https://github.com/End-to-end-provenance/RDataTracker/wiki/Background-Literature)
to learn more about provenance collection from a scientific perspecitive.

Other tools that utilise provenance may be found
[here](https://github.com/End-to-end-provenance).

## Why provDebugR?
A typical debugger pauses execution at pre-determined points so a user can observe 
the execution environment. This functionality allows them to step through the program 
line-by-line and watch how variables change and observe where the flow of control leads.

The main drawback about this is that the user is unable to go back to lines prior to the
line they are on. If they wanted to observe the execution environment at a previous
line, they must start the debugger from the beginning of execution. This makes it
hard to debug non-deterministic scripts.

By utilising provenance, provDebugR allows the user to observe the execution environment
at any point in the script's last execution, jumping back and forth between lines, as
many times as they wish. It also allows for the tracking of variables throughout the 
script to observe type changes and connections to other variables.