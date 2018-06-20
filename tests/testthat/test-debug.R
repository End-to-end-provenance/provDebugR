library(RDataTracker)
library(provParseR)
library(provGraphR)
library(provDebugR)
library(igraph)

# Loading test data
test.data <- system.file("testdata", "test.json", package = "provDebugR")
debug.prov(test.data)

# context("R or Rmd input")
# R script not working

context("JSON input")
envi.df <- get.environment()
expect_match(class(envi.df), "data.frame")
expect_match(typeof(envi.df$value), "character")

context("Graph")
s <- get.spine("p11")

context("Debugging Lineage")
dl.df <- debug.lineage("x")
expect_match(dl.df$x$code[1], "y <- 2")
expect_equal(dl.df$x$line[1], 3)
dl.df <- debug.lineage("y", forward = T)
expect_match(dl.df$y$code[1], "y <- 2")
expect_match(dl.df$y$code[6], "x <- b + y")
