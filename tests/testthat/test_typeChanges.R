library(testthat)
library(provDebugR)

context("debug.type.changes")

# === HELPER FUNCTIONS ======================================================= #

# helper function to create the list of expected tables
get.expected <- function()
{
	# container change
	d <- data.frame(value = as.character(c(4,4)),
					container = c("vector", "list"),
					dimension = as.character(c(1,1)),
					type = c("numeric", NA),
					code = c('d <- 4', 'd <- as.list(d)'),
					scriptNum = c(1,1),
					startLine = c(12,13),
					stringsAsFactors = FALSE)
	
	# dimension change
	# omit value column as it's too long
	e <- data.frame(container = c("matrix", "matrix"),
					dimension = c("4,25", "5,20"),
					type = c("integer", "integer"),
					code = c('e <- matrix(c(1:100), 4)', 'e <- matrix(c(1:100), 5)'),
					scriptNum = c(1,1),
					startLine = c(16,17),
					stringsAsFactors = FALSE)
	
	# type change
	f <- data.frame(value = as.character(c(5,5)),
					container = c("vector", "vector"),
					dimension = as.character(c(1,1)),
					type = c("numeric", "integer"),
					code = c('f <- 5', 'f <- as.integer(f)'),
					scriptNum = c(1,1),
					startLine = c(20,21),
					stringsAsFactors = FALSE)
	
	# multiple valType changes in sequence
	g <- data.frame(value = c('6', '"six"', 'TRUE'),
					container = c("vector", "vector", "vector"),
					dimension = as.character(c(1,1,1)),
					type = c("numeric", "character", "logical"),
					code = c('g <- 6', 'g <- "six"', 'g <- TRUE'),
					scriptNum = c(1,1,1),
					startLine = c(24,25,26),
					stringsAsFactors = FALSE)
	
	# multiple valType changes, with no type changes
	h <- data.frame(value = c('TRUE', '"seven"', '"eight"', '8'),
					container = c("vector", "vector", "vector", "vector"),
					dimension = as.character(c(1,1,1,1)),
					type = c("logical", "character", "character", "integer"),
					code = c('h <- TRUE', 'h <- "seven"', 'h <- "eight"', 'h <- 8L'),
					scriptNum = c(1,1,1,1),
					startLine = c(30,31,32,33),
					stringsAsFactors = FALSE)
	
	# special data types
	# also omit value and code column due to length
	s <- data.frame(container = as.character(c(NA,NA,NA,NA,NA)),
					dimension = as.character(c(NA,NA,NA,NA,NA)),
					type = c('null', 'environment', 'function', 'factor', 'POSIXct'),
					scriptNum = c(1,1,1,1,1),
					startLine = c(38,39,40,41,42),
					stringsAsFactors = FALSE)
	
	# combine
	type.changes <- list(d,e,f,g,h,s)
	names(type.changes) <- c("d", "e", "f", "g", "h", "s")
	
	return(type.changes)
}

# === THE TESTS ============================================================== #

# no provenance
test_that("debug.type.changes - no/empty provenance", 
{
	# clean debug environment of provDebugR first to ensure inital state
	provDebugR:::.clear()
	
	# initialisation not run
	expect_false(provDebugR:::.debug.env$has.graph)
	expect_error(debug.type.changes())
	
	# empty provenance
	c0 <- system.file("testdata", "empty.json", package = "provDebugR")
	expect_error(prov.debug.file(c0))
	expect_false(provDebugR:::.debug.env$has.graph)
	expect_error(debug.type.changes())
})

# no data nodes
test_that("debug.type.changes - no data nodes",
{
	skip("debug.type.changes - no data nodes")
	
	json <- system.file("testdata", "noDataNodes.json", package = "provDebugR")
	
	provDebugR:::.clear()
	expect_warning(prov.debug.file(json))   # warning is due to deleted prov folder
	
	c2 <- utils::capture.output(c1 <- debug.type.changes())
	
	expect_null(c1)
	expect_true(nchar(paste(c2, collapse='\n')) > 0)
})

# no variables
test_that("debug.type.changes - no variables",
{
	json <- system.file("testdata", "noVars.json", package = "provDebugR")
	
	provDebugR:::.clear()
	expect_warning(prov.debug.file(json))   # warning is due to deleted prov folder
	
	c2 <- utils::capture.output(c1 <- debug.type.changes())
	
	expect_null(c1)
	expect_true(nchar(paste(c2, collapse='\n')) > 0)
})

# debug.type.changes tests
json <- system.file("testdata", "typeChanges.json", package = "provDebugR")

provDebugR:::.clear()
expect_warning(prov.debug.file(json))   # warning is due to deleted prov folder

expected <- get.expected()

# debug.type.changes - no paramters
test_that("debug.type.changes - no parameters", 
{
	c1 <- debug.type.changes()
	c1$e <- c1$e[ ,-1]          # omit value column
	c1$s <- c1$s[ , c(-1,-5)]   # omit value and code column
	
	expect_equivalent(c1, expected)
})

# debug.type.changes - all valid variables queried (single)
test_that("debug.type.changes - all valid variables queried (single)",
{
	# queries
	q1 <- "d"
	q2 <- "e"
	q3 <- "f"
	q4 <- "g"
	q5 <- "h"
	q6 <- "s"
	
	# test cases
	c1 <- debug.type.changes(var = q1)[[1]]
	c2 <- debug.type.changes(var = q2)[[1]][ ,-1]          # omit value column
	c3 <- debug.type.changes(var = q3)[[1]]
	c4 <- debug.type.changes(var = q4)[[1]]
	c5 <- debug.type.changes(var = q5)[[1]]
	c6 <- debug.type.changes(var = q6)[[1]][ , c(-1,-5)]   # omit value and code column
	
	# test
	expect_equivalent(c1, expected$d)
	expect_equivalent(c2, expected$e)
	expect_equivalent(c3, expected$f)
	expect_equivalent(c4, expected$g)
	expect_equivalent(c5, expected$h)
	expect_equivalent(c6, expected$s)
})

# debug.type.changes - all valid variables queried (multiple)
test_that("debug.type.changes - all valid variables queried (multiple)",
{
	# queries
	q1 <- c("d", "f", "g")   # multiple variables
	q2 <- c("f", "h", "d")   # multiple variables (in different order)
	q3 <- c("d", "d", "f")   # repeated query
	
	# test cases
	c1 <- debug.type.changes(var = q1)
	c2 <- debug.type.changes(var = q2)
	c3 <- debug.type.changes(var = q3)
	
	# expected
	e1 <- expected[q1]
	e2 <- expected[q2]
	e3 <- expected[c("d", "f")]
	
	# test
	expect_equivalent(c1, e1)
	expect_equivalent(c2, e2)
	expect_equivalent(c3, e3)
})

# debug.type.changes - all invalid variables queried
test_that("debug.type.changes - all invalid variables queried",
{
	# queries
	q1 <- "invalid"      # variable does not exist
	q2 <- "Rplots.pdf"   # data node with name exists, but is not a variable
	q3 <- "a"            # no type change (just single assignment)
	q4 <- "cc"           # no type change (value does change)
	
	# test cases (both returned value and message output)
	c2 <- utils::capture.output(c1 <- debug.type.changes(var = q1))
	c4 <- utils::capture.output(c3 <- debug.type.changes(var = q2))
	c6 <- utils::capture.output(c5 <- debug.type.changes(var = q3))
	c8 <- utils::capture.output(c7 <- debug.type.changes(var = q4))
	
	# test returned values
	expect_null(c1)
	expect_null(c3)
	expect_null(c5)
	expect_null(c7)
	
	# test message output
	c2 <- paste(c2, collapse = "\n")
	c4 <- paste(c4, collapse = "\n")
	c6 <- paste(c6, collapse = "\n")
	c8 <- paste(c8, collapse = "\n")
	
	expect_true(nchar(c2) > 0)
	expect_true(nchar(c4) > 0)
	expect_true(nchar(c6) > 0)
	expect_true(nchar(c8) > 0)
})

# debug.type.changes - some valid, some invalid variables queried
test_that("debug.type.changes - some valid, some invalid vars",
{
	# queries
	q1 <- c("invalid", "h", "a", "g")
	
	# test cases
	c1 <- debug.type.changes(var = q1)
	
	# test expected return values
	e1 <- expected[c("h", "g")]
	expect_equivalent(c1, e1)
})
