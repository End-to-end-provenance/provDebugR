library(RDataTracker)
library(provParseR)
library(provGraphR)
library(provDebugR)

context("test-debug.R")

# Loading test data
test.data <- system.file("testdata", "test.json", package = "provDebugR")
debug.init(test.data)

context("if input is R or Rmd")
# R script not working

context("if input is json")
envi.df <- get.environment

#string split ->
#call prov.parse access functions
