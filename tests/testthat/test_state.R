library(testthat)
library(provDebugR)

context("debug.state")

# === THE TESTS ============================================================== #

# no provenance
test_that("debug.state - no/empty provenance", 
{
	# clean debug environment of provDebugR first to ensure inital state
	provDebugR:::.clear()
	
	# initialisation not run
	expect_false(provDebugR:::.debug.env$has.graph)
	expect_error(debug.state())
	
	# empty provenance
	c0 <- system.file("testdata", "empty.json", package = "provDebugR")
	expect_error(prov.debug.file(c0))
	expect_false(provDebugR:::.debug.env$has.graph)
	expect_error(debug.state())
})

# no data nodes
test_that("debug.state - no data nodes",
{
	skip("debug.state - no data nodes")
	
	json <- system.file("testdata", "noDataNodes.json", package = "provDebugR")
	
	provDebugR:::.clear()
	expect_warning(prov.debug.file(json))   # warning is due to deleted prov folder
	
	c2 <- utils::capture.output(c1 <- debug.state())
	
	expect_null(c1)
	expect_true(nchar(paste(c2, collapse='\n')) > 0)
})

# no variables
test_that("debug.state - no variables",
{
	json <- system.file("testdata", "noVars.json", package = "provDebugR")
	
	provDebugR:::.clear()
	expect_warning(prov.debug.file(json))   # warning is due to deleted prov folder
	
	c2 <- utils::capture.output(c1 <- debug.state())
	
	expect_null(c1)
	expect_true(nchar(paste(c2, collapse='\n')) > 0)
})

# debug.state (line queries)
test_that("debug.state (line queries)",
{
	json <- system.file("testdata", "typeChanges.json", package = "provDebugR")
	provDebugR:::.clear()
	expect_warning(prov.debug.file(json))   # warning from deleted prov folder
	
	# CASES
	# columns to be compared: name, scriptNum, startLine (cols 1,6,8)
	c2 <- utils::capture.output(                      # no state
		c1 <- debug.state(0))
	c4 <- utils::capture.output(                      # no parameters given
		c3 <- debug.state()[[1]][ , c(1,6,8)])
	
	c6 <- utils::capture.output(                      # var does not have its final value
		c5 <- debug.state(20)[[1]][ , c(1,6,8)])
	c8 <- utils::capture.output(                      # queried line does not have an operation
		c7 <- debug.state('10')[[1]][ , c(1,6,8)])
	
	c10 <- utils::capture.output(                     # invalid line query
		c9 <- debug.state(5.5)[[1]][ , c(1,6,8)])
	c12 <- utils::capture.output(                     # multiple invalid line queries
		c11 <- debug.state(5.5,NA)[[1]][ , c(1,6,8)])
	
	c14 <- utils::capture.output(                     # multiple valid line queries
		c13 <- debug.state(10,20))
	c13$`1` <- c13$`1`[ , c(1,6,8)]
	c13$`2` <- c13$`2`[ , c(1,6,8)]
	
	c16 <- utils::capture.output(                     # valid and invalid queries
		c15 <- debug.state(10,5.5,20))
	c15$`1` <- c15$`1`[ , c(1,6,8)]
	c15$`2` <- c15$`2`[ , c(1,6,8)]
	
	
	# EXPECTED
	e3 <- data.frame(name = c('a','cc','d','e','f','g','h','s'),         # state at end of execution
					 scriptNum = rep(1L,8),
					 startLine = as.integer(c(2,9,13,17,21,26,34,42)),
					 stringsAsFactors = FALSE)
	
	e5 <- data.frame(name = c('a','cc','d','e','f'),                     # line 20
					 scriptNum = rep(1L,5),
					 startLine = as.integer(c(2,9,13,17,20)),
					 stringsAsFactors = FALSE)
	e7 <- data.frame(name = c('a','cc'),                                 # line 10
					 scriptNum = rep(1L,2),
					 startLine = as.integer(c(2,9)),
					 stringsAsFactors = FALSE)
	
	e13 <- list(`1`= e7, `2`= e5)   # multiple valid queries
	
	
	# TEST
	expect_null(c1)               # no state
	expect_equivalent(c3, e3)     # no parameters given
	expect_equivalent(c5, e5)     # var does not have its final value
	expect_equivalent(c7, e7)     # queried line does not have an operation
	expect_equivalent(c9, e3)     # invalid line query
	expect_equivalent(c11, e3)    # multiple invalid line queries
	expect_equivalent(c13, e13)   # multiple valid line queries
	expect_equivalent(c15, e13)   # valid and invalid queries
	
	expect_true(nchar(paste(c2, collapse='\n')) > 0)
	expect_true(nchar(paste(c4, collapse='\n')) > 0)
	expect_true(nchar(paste(c6, collapse='\n')) > 0)
	expect_true(nchar(paste(c8, collapse='\n')) > 0)
	expect_true(nchar(paste(c10, collapse='\n')) > 0)
	expect_true(nchar(paste(c12, collapse='\n')) > 0)
	expect_true(nchar(paste(c14, collapse='\n')) > 0)
	expect_true(nchar(paste(c16, collapse='\n')) > 0)
})

# debug.state (multiple scripts)
test_that("debug.state (multiple scripts)",
{
	json <- system.file("testdata", "exceptions.json", package = "provDebugR")
	provDebugR:::.clear()
	expect_warning(prov.debug.file(json))   # warning due to deleted prov folder
	
	# CASES
	c2 <- utils::capture.output(                     # line does not assign to variable
		c1 <- debug.state(6, script.num = 2))
	c4 <- utils::capture.output(                     # out of bounds: start of execution
		c3 <- debug.state(0, script.num = 1))
	c6 <- utils::capture.output(                     # out of bounds: end of execution
		c5 <- debug.state(15, script.num = "3"))
	
	c8 <- utils::capture.output(                     # more than 1 script number queried
		c7 <- debug.state(6, script.num = c(1,2)))
	c10 <- utils::capture.output(                    # script number is not an integer
		c9 <- debug.state(6, script.num = 1.2))
	
	c12 <- utils::capture.output(
		c11 <- debug.state(6, script.num = "all"))   # script.num = "all"
	
	# extract columns to be compared: 
	# name, scriptNum, startLine (cols 1,6,8)
	c1 <- c1[[1]][ , c(1,6,8)]
	c5 <- c5[[1]][ , c(1,6,8)]
	
	c7$`1` <- c7$`1`[ , c(1,6,8)]
	c7$`2` <- c7$`2`[ , c(1,6,8)]
	
	c9 <- c9[[1]][ , c(1,6,8)]
	
	c11$`1` <- c11$`1`[ , c(1,6,8)]
	c11$`2` <- c11$`2`[ , c(1,6,8)]
	c11$`3` <- c11$`3`[ , c(1,6,8)]
	
	
	# EXPECTED
	e1 <- data.frame(name = 'col1',                           # line 6, script 2
					 scriptNum = 2L,
					 startLine = 1L,
					 stringsAsFactors = FALSE)
	
	e5 <- data.frame(name = c('col1', 'get.error.df','df'),   # at end of execution
					 scriptNum = as.integer(c(2,3,3)),
					 startLine = as.integer(c(1,1,8)),
					 stringsAsFactors = FALSE)
	
	e7.1 <- data.frame(name = 'col1',                         # more than 1 script number queried
					   scriptNum = 1L,
					   startLine = 3L,
					   stringsAsFactors = FALSE)
	e7.2 <- data.frame(name = 'col1',
					   scriptNum = 2L,
					   startLine = 1L,
					   stringsAsFactors = FALSE)
	e7 <- list(`1` = e7.1, `2` = e7.2)
	
	e11.3 <- data.frame(name = c("col1", "get.error.df"),     # script.num = "all"
						scriptNum = as.integer(c(2,3)),
						startLine = as.integer(c(1,1)),
						stringsAsFactors = FALSE)
	e11 <- list(`1` = e7.1, `2` = e7.2, `3` = e11.3)
	
	
	# TEST
	expect_equivalent(c1, e1)    # line does not assign to variable
	expect_null(c3)              # out of bounds: start of execution
	expect_equivalent(c5, e5)    # out of bounds: end of execution
	expect_equivalent(c7, e7)    # more than 1 script number queried
	expect_equivalent(c9, e5)    # script number is not an integer
	expect_equivalent(c11,e11)   # script.num = "all"
	
	expect_true(nchar(paste(c2, collapse='\n')) > 0)
	expect_true(nchar(paste(c4, collapse='\n')) > 0)
	expect_true(nchar(paste(c6, collapse='\n')) > 0)
	expect_true(nchar(paste(c8, collapse='\n')) > 0)
	expect_true(nchar(paste(c10, collapse='\n')) > 0)
	expect_true(nchar(paste(c12, collapse='\n')) > 0)
	
	skip("debug.state - cases which includes Start and Finish nodes")
})

# debug.state (fromEnv variables)
test_that("debug.state (fromEnv variables)",
{
	json <- system.file("testdata", "fromEnv.json", package = "provDebugR")
	provDebugR:::.clear()
	expect_warning(prov.debug.file(json))   # warning from deleted prov folder
	
	# CASES
	# columns to be compared: name, scriptNum, startLine (cols 1,6,8)
	c2 <- utils::capture.output(                                  # at beginning of execution
		c1 <- debug.state(0, script.num = 1)[[1]][ , c(1,6,8)])
	c4 <- utils::capture.output(                                  # at end of execution
		c3 <- debug.state()[[1]][ , c(1,6,8)])
	
	# EXPECTED
	e1 <- data.frame(name = c('a', 'b'),                 # at beginning of execution
					 scriptNum = c(NA,NA),
					 startLine = c(NA,NA),
					 stringsAsFactors = FALSE)
	
	# state at end of execution
	e3 <- data.frame(name = c('a','b','d','vector.1','e','f','vector.2','vector.3','vector.4','g'),
					 scriptNum = as.integer(c(NA,NA,1,1,1,1,1,1,1,1)),
					 startLine = as.integer(c(NA,NA,1,2,5,6,7,18,19,25)),
					 stringsAsFactors = FALSE)
	
	# TEST
	expect_equivalent(c1, e1)
	expect_equivalent(c3, e3)
	
	expect_true(nchar(paste(c2, collapse='\n')) > 0)
	expect_true(nchar(paste(c4, collapse='\n')) > 0)
})

# === TESTING HELPER FUNCTIONS =============================================== #

# .get.valid.query.state
test_that("debug.state - .get.valid.query.state",
{
	pos.nodes <- data.frame(p.id = as.integer(c(1:10)),
							startLine = as.integer(c(1:10)),
							scriptNum = as.integer(rep(c(1,2), each = 5)),
							stringsAsFactors = FALSE)
	
	# QUERIES
	q1 <- NULL                                           # invalid cases
	q2 <- data.frame(startLine = 11.5, 
					 scriptNum = 1L,
					 stringsAsFactors = FALSE)
	q3 <- data.frame(startLine = 1L, 
					 scriptNum = 2.5,
					 stringsAsFactors = FALSE)
	q4 <- data.frame(startLine = NA, 
					 scriptNum = 2L,
					 stringsAsFactors = FALSE)
	
	q5 <- data.frame(startLine = as.integer(c(2,6)),     # all valid
					 scriptNum = as.character(c(1,2)),
					 stringsAsFactors = FALSE)
	q6 <- data.frame(startLine = as.character(c(3,7)), 
					 scriptNum = as.integer(c(1,2)),
					 stringsAsFactors = FALSE)
	
	q7 <- data.frame(startLine = c(1, 2.6, 50),          # valid and invalid
					 scriptNum = as.integer(c(1,2,2)),
					 stringsAsFactors = FALSE)
	
	# CASES
	c1 <- provDebugR:::.get.valid.query.state(pos.nodes, q1)   # invalid cases
	c2 <- provDebugR:::.get.valid.query.state(pos.nodes, q2)
	c3 <- provDebugR:::.get.valid.query.state(pos.nodes, q3)
	c4 <- provDebugR:::.get.valid.query.state(pos.nodes, q4)
	c5 <- provDebugR:::.get.valid.query.state(pos.nodes, q5)   # all valid
	c6 <- provDebugR:::.get.valid.query.state(pos.nodes, q6)
	c7 <- provDebugR:::.get.valid.query.state(pos.nodes, q7)   # valid and invalid
	
	# EXPECTED
	e5 <- q5             # all valid
	e6 <- q6
	e7 <- q7[c(1,3), ]   # valid and invalid
	
	# TEST
	expect_null(c1)             # invalid cases
	expect_null(c2)
	expect_null(c3)
	expect_null(c4)
	expect_equivalent(c5, e5)   # all valid
	expect_equivalent(c6, e6)
	expect_equivalent(c7, e7)   # valid and invalid
})

# .get.closest.proc
test_that("debug.state - .get.closest.proc",
{
	json <- system.file("testdata", "exceptions.json", package = "provDebugR")
	provDebugR:::.clear()
	expect_warning(prov.debug.file(json))   # warning due to deleted prov folder
	
	# pos.nodes
	pos.nodes <- provDebugR:::.get.pos.line(provDebugR:::.debug.env$proc.nodes)
	
	# cases
	c1 <- provDebugR:::.get.closest.proc(pos.nodes,3,1)    # script and line numbers can be found
	c2 <- provDebugR:::.get.closest.proc(pos.nodes,0,1)    # out of bounds - start of script
	c3 <- provDebugR:::.get.closest.proc(pos.nodes,15,3)   # out of bounds - end of script
	c4 <- provDebugR:::.get.closest.proc(pos.nodes,5,2)    # line not found - in middle of a script
	
	# test
	expect_equal(c1, 'p3')
	expect_null(c2)
	expect_equal(c3, 'p11')
	expect_equal(c4, 'p5')
	
	skip(".get.closest.proc - cases which includes Start and Finish nodes")
})

# .get.last.var
test_that("debug.state - .get.last.var",
{
	json <- system.file("testdata", "exceptions.json", package = "provDebugR")
	provDebugR:::.clear()
	expect_warning(prov.debug.file(json))   # warning due to deleted prov folder
	
	# pos.nodes
	pos.nodes <- provDebugR:::.get.pos.line(provDebugR:::.debug.env$proc.nodes)
	
	# CASES
	c1 <- provDebugR:::.get.last.var(pos.nodes,NULL)   # p.id is null
	c2 <- provDebugR:::.get.last.var(pos.nodes,"p2")   # no output variables found
	c3 <- provDebugR:::.get.last.var(pos.nodes,"p5")   # output variable at that procedure node
	c4 <- provDebugR:::.get.last.var(pos.nodes,"p6")   # output variable not at that procedure node
	
	# TEST
	expect_null(c1)
	expect_null(c2)
	expect_equal(c3, "d3")
	expect_equal(c4, "d3")
})

# .get.state (multiple var reassignment)
test_that("debug.state - .get.state (no fromEnv nodes)",
{
	json <- system.file("testdata", "typeChanges.json", package = "provDebugR")
	provDebugR:::.clear()
	expect_warning(prov.debug.file(json))   # warning from deleted prov folder
	
	# CASES
	c1 <- provDebugR:::.get.state(NULL)    # no state
	c2 <- provDebugR:::.get.state('d17')   # not the final value of a variable
	c3 <- provDebugR:::.get.state('d25')   # multiple reassignments of variables
	
	# EXPECTED
	e2 <- paste('d', c(1,5,7,9,11,14,17), sep='')
	e3 <- paste('d', c(1,5,7,9,11,14,20,25), sep='')
	
	# TEST
	expect_null(c1)
	expect_equivalent(c2, e2)
	expect_equivalent(c3, e3)
})

# .get.state (with fromEnv nodes)
test_that("debug.state - .get.state (with fromEnv nodes)",
{
	json <- system.file("testdata", "fromEnv.json", package = "provDebugR")
	provDebugR:::.clear()
	expect_warning(prov.debug.file(json))   # warning from deleted prov folder
	
	# CASES
	c1 <- provDebugR:::.get.state(NULL)    # fromEnv vars only
	c2 <- provDebugR:::.get.state("d15")   # no reassignment of variables
	
	# EXPECTED
	e1 <- c('d2','d19')
	e2 <- paste('d', c(2,19,1,3,5,6,7,14,15), sep='')
	
	# TEST
	expect_equivalent(c1, e1)
	expect_equivalent(c2, e2)
})
