library(rdtLite)

col1 <- c(1:11)

prov.source(system.file("testscripts", "source_warning.r", package = "provDebugR"))
prov.source(system.file("testscripts", "source_error.r", package = "provDebugR"))

print("exceptions test case complete")
