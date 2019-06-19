# author: Elizabeth Fong
library(testthat)
library(provDebugR)
library(provParseR)

context("Browser")

# no provenance
test_that("no/empty provenance", {
	
	# debugger has not been initialised
	expect_error(debug.browser())
	
	# no provenance (no proc nodes)
	json <- system.file("testdata", "empty.json", package = "provDebugR")
	expect_error(debug.init(json))
	expect_error(debug.browser())
})

# step-in table
test_that("step-in table", {
	
	# cases
	# 0, 1, 3 step-in locations
	c0 <- system.file("testdata", "stepin0.json", package = "provDebugR")
	#c1 <- system.file("testdata", "stepin1.json", package = "provDebugR")
	#c3 <- system.file("testdata", "stepin3.json", package = "provDebugR")
	
	t0 <- .get.step.in.table(get.proc.nodes(prov.parse(c0)))
	#t1 <- .get.step.in.table(get.proc.nodes(prov.parse(c1)))
	#t3 <- .get.step.in.table(get.proc.nodes(prov.parse(c3)))
	
	# expected
	#e1 <- read.csv(system.file("testexpected", "stepin1.csv", package = "provDebugR"))
	#e3 <- read.csv(system.file("testexpected", "stepin3.csv", package = "provDebugR"))
	
	# test
	expect_true(nrow(.get.step.in.table(proc.nodes)) == 0)
	#expect_equivalent(t1, e1)
	e#xpect_equivalent(t3, e3)
})
