library(rdtLite)

f1 <- function(x)
{
	a <- 5
	b <- 6
	x <- a + b + x
	return(x)
}

prov.source("testscripts/source_notInProv.r")

var2 <- f2(24)
print(var2)
