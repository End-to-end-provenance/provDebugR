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
	
	t0 <- provDebugR:::.get.step.in.table(get.proc.nodes(prov.parse(c0)))
	#t1 <- provDebugR:::.get.step.in.table(get.proc.nodes(prov.parse(c1)))
	#t3 <- provDebugR:::.get.step.in.table(get.proc.nodes(prov.parse(c3)))
	
	# expected
	#e1 <- read.csv(system.file("testexpected", "stepin1.csv", package = "provDebugR"))
	#e3 <- read.csv(system.file("testexpected", "stepin3.csv", package = "provDebugR"))
	
	# test
	expect_true(nrow(t0) == 0)
	#expect_equivalent(t1, e1)
	#expect_equivalent(t3, e3)
})


test_that( "change line", {
	
	
	
})






# params:
# input, var.env, current.script, pos.lines, proc.nodes, scripts

# move forward
# TODO: also need to check returned table as well!
test_that("forward movement", {
	
	test.file <- system.file("testdata", "stepin3.json", package = "provDebugR")
	debug.init(test.file)

	prov <- provParseR::prov.parse(test.file)
	current.script <- 1

	proc.nodes <- provParseR::get.proc.nodes(provParseR::prov.parse(test.file))
	pos.lines <- stats::na.omit(proc.nodes$startLine)

	scripts <- provParseR::get.scripts(prov)

	var.env <- new.env(parent = emptyenv())
	var.env$call.stack <- list()
	var.env$lineIndex <- 2		# mirroring value of var.env in .read.input (it's 2, not 1)
	var.env$vars <- NA
	
	returned <- NULL
	
	output <- capture.output(returned <- provDebugR:::.moveForward('n', var.env, current.script, pos.lines, proc.nodes, scripts))
	
	expect_identical(output, "3: b <- 15")
	
	#current.script <- returned$new.script
	#proc.nodes <- returned$new.nodes
	#pos.lines <- returned$new.lines
	
	#var.env$b <- 15
	#var.env$lineIndex <- 6
	#var.env$vars <- 'b'
	
	#output <- capture.output(returned <- provDebugR:::.moveForward('n', var.env, current.script, pos.lines, proc.nodes, scripts))
	
	#output.2 <<- output
	
	#expect_identical(output, "6: d <- 35")
	
})

# move backward
