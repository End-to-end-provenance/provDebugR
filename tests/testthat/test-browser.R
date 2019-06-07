# author: Elizabeth Fong
library(testthat)
library(provDebugR)

# no provenance
context("Browser")

test_that("no/empty provenance", {
	# no provenance/debugger has not been initialised
	expect_error(debug.browser())
	
	json <- system.file("testdata/cases", "empty.json", package = "provDebugR")
	expect_error(debug.init(json))
	expect_error(debug.browser())
})


# sourced scripts
#sourced.scripts.data <- system.file("testdata/testSourcedScripts", "prov.json", package = "provDebugR")
#sourced.scripts.data <- "../../inst/testdata/testSourcedScripts/prov.json"
#sourced.scripts.proc <- provParseR::get.proc.nodes(provParseR::prov.parse(sourced.scripts.data))
#
#
# .get.step.in.table
#test_that("check step.in table", {
#	
#	# has proc nodes, but no step-in nodes
#	#json <- system.file("testdata/testEmpty", "noStepIn.json", package = "provDebugR")
#	json <- "../../inst/testdata/testEmpty/noStepIn.json"
#	proc <- provParseR::get.proc.nodes(provParseR::prov.parse(json))
#	expect_identical(.get.step.in.table(proc), data.frame())
#	
#	# with sourced scripts and user-defined functions
#	#expected.file <- system.file("testdata/testSourcedScripts", "stepInTable.csv", package = "provDebugR")
#	expected.file <- "../../inst/testdata/testSourcedScripts/stepInTable.csv"
#	expected.val <- read.csv(expected.file, header=TRUE, row.names=1, stringsAsFactors=FALSE)
#	expect_identical(.get.step.in.table(sourced.scripts.proc), expected.val)
#})
#
# .read.input
#test_that("read input", {
#	
#	# quit
#	input <- "Q"
#	expect_identical(.read.input(input), "Quitting")
#	expect_true(.read.input(input))
#	
#	# f
#})
#
#
# .change.line
#
#.clear.environment
#test_that("clear environment", {
#	expect_true(provDebugR:::.debug.env$has.graph)
#})
#
#.load.variables
#.moveForward
#.moveBackward
#.stepIn
