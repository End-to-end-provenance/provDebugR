library(RDataTracker)
library(provParseR)
library(provGraphR)
library(provDebugR)

# Loading test data
test.data <- system.file("testdata", "test.json", package = "provDebugR")
debug.init(test.data)

# context("R or Rmd input")
# R script not working

context("JSON input")
envi.df <- get.environment()
expect_match(class(envi.df), "data.frame")
expect_match(typeof(envi.df$value), "character")

context("Graph")
s <- get.spine("p11")
expect_match(class(s), "character")
expect_equal(length(s), 2)
