library(RDataTracker)
library(provParseR)
library(provGraphR)
library(provDebugR)

# Loading test data
test.data <- system.file("testdata", "test.json", package = "provDebugR")
debug.init(test.data)

# context("If input is R or Rmd")
# R script not working

context("If input is json")
envi.df <- get.environment()
expect_match(class(envi.df), "data.frame")
expect_match(typeof(envi.df$value), "character")
