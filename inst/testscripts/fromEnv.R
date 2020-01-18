# before prov collection starts
a <- 1
b <- 5
cc <- 7

# start prov collection: use a, b, but not cc
library(rdtLite)
prov.init(prov.dir = "testdata", snapshot.size="10")

prov.source("testscripts/source_fromEnv.r")

prov.quit()
