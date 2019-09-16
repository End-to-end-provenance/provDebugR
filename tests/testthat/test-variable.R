library(testthat)
library(provDebugR)

context("Variable")

json <- system.file("testdata", "test.json", package = "provDebugR")
prov.debug.file(json)

test_that("debug.variable",
{
	
})