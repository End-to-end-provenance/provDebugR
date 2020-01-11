library(testthat)
library(provDebugR)

context("debug.variable")

# no provenance
test_that("debug.variable - no/empty provenance", 
{
	# clean debug environment of provDebugR first to ensure inital state
	provDebugR:::.clear()
	
	# initialisation not run
	expect_false(provDebugR:::.debug.env$has.graph)
	expect_error(debug.variable("x"))
	
	# empty provenance
	c0 <- system.file("testdata", "empty.json", package = "provDebugR")
	expect_error(prov.debug.file(c0))
	expect_false(provDebugR:::.debug.env$has.graph)
	expect_error(debug.variable("x"))
})

# general case
# no param
# all
# invalid script
# invalid vars
# invalid valtypes


# no variables

# .get.pos.vars

# .get.valid.var

# .get.query.var
test_that("debug.variable - .get.query.var (valid queries)",
{	
	# 1 object queried
	c1 <- provDebugR:::.get.query.var("x")
	
	e1 <- data.frame(name="x", valType=NA, startLine=NA, scriptNum=1, 
					 stringsAsFactors = FALSE)
	expect_equivalent(c1, e1)
	
	# 1 object queried, multiple start line numbers
	c2 <- provDebugR:::.get.query.var("x", start.line = c(4,2,7))
	
	e2 <- data.frame(name = c("x","x","x"), 
					 valType = c(NA,NA,NA),
					 startLine = c(4,2,7), 
					 scriptNum = c(1,1,1), 
					 stringsAsFactors = FALSE)
	expect_equivalent(c2, e2)
	
	# multiple objects queried, 1 start line number
	c3 <- provDebugR:::.get.query.var("x", "y", start.line = 5)
	
	e3 <- data.frame(name = c("x","y"), 
					 valType = c(NA,NA),
					 startLine = c(5,5), 
					 scriptNum = c(1,1), 
					 stringsAsFactors = FALSE)
	expect_equivalent(c3, e3)
	
	# same number of objects and start line numbers
	c4 <- provDebugR:::.get.query.var("x", c("z", "y"), start.line = c(3:5))
	
	e4 <- data.frame(name = c("x","z","y"), 
					 valType = c(NA,NA,NA),
					 startLine = c(3:5), 
					 scriptNum = c(1,1,1), 
					 stringsAsFactors = FALSE)
	expect_equivalent(c4, e4)
	
	# same number of objects and start line numbers, with NA as a line number
	c5 <- provDebugR:::.get.query.var("x", "y", "z", start.line = c(3,NA,5))
	
	e5 <- data.frame(name = c("x","y","z"), 
					 valType = c(NA,NA,NA),
					 startLine = c(3,NA,5), 
					 scriptNum = c(1,1,1), 
					 stringsAsFactors = FALSE)
	expect_equivalent(c5, e5)
	
	# val.type and script.num not their default values
	c6 <- provDebugR:::.get.query.var("x", "y", 
									  val.type = "logical", 
									  start.line = c(NA,5), 
									  script.num = 2)

	e6 <- data.frame(name = c("x","y"), 
					 valType = c("logical", "logical"),
					 startLine = c(NA,5), 
					 scriptNum = c(2,2), 
					 stringsAsFactors = FALSE)
	expect_equivalent(c6, e6)
})

test_that("debug.variable - .get.query.var (invalid queries)",
{
	# queried more than 1 script numbers
	expect_warning(c1 <- provDebugR:::.get.query.var(script.num = c(1:2)))
	expect_null(c1)
	
	# queried more than 1 valTypes
	expect_warning(c2 <- provDebugR:::.get.query.var(val.type = c("logical", "character")))
	expect_null(c2)
	
	# objects and start line numbers queried are greater than 1, and do not match
	obj <- c("x", "y")
	lines <- c(1:3)
	
	expect_warning(c3 <- provDebugR:::.get.query.var(obj, start.line = lines))
	expect_null(c3)
})

# .get.output.var (?)
