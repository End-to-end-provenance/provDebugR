library(RDataTracker)
library(provParseR)
library(provGraphR)
library(provDebugR)
library(igraph)

# Loading test data
context("Initialize Debugger")
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
expect_equal(length(s), 5)
expect_match(s[3], "p6")

context("Debugging Lineage")
dl.df <- debug.lineage("x")
expect_match(dl.df$x$code[1], "y <- 2")
expect_equal(dl.df$x$line[1], 3)
dl.df <- debug.lineage("y", forward = T)
expect_match(dl.df$y$code[1], "y <- 2")
expect_match(dl.df$y$code[6], "x <- b + y", fixed = T)
dl.dfs <- debug.lineage("x", "y")
expect_equal(length(dl.dfs), 2)

context("Debug Warnings and Errors")
debug.error <- debug.error.trace()
expect_match(class(debug.error),"data.frame")
expect_equal(debug.error$line[2], 24)

debug.warn <- debug.warning.trace(1,2)
expect_equal(debug.warn[[2]]$line, 22)
expect_equal(length(debug.warn), 2)

context("Debug Variable Type")
var.type.results <- debug.variable.type()
expect_equal(length(var.type.results), 9)
expect_match(var.type.results[3], "y")
var.type <- debug.variable.type(var.type.results)
expect_equal(length(var.type), length(var.type.results))
expect_match(var.type[[1]]$line[1], "1")
expect_match(var.type[[1]]$line[2], "10")

context("Debug From Line")
from.lines <- debug.from.line()
expect_match(from.lines$var[[3]], "z")
expect_match(from.lines$type[[8]], "data_frame 4x3")
from.lines <- debug.from.line(1, 2:5, 8)
expect_equal(length(from.lines), 6)
expect_match(from.lines$`5`$type, "double")
