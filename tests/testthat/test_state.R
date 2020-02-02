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
	expect_error(debug.state(10))
	
	# empty provenance
	c0 <- system.file("testdata", "empty.json", package = "provDebugR")
	expect_error(prov.debug.file(c0))
	expect_false(provDebugR:::.debug.env$has.graph)
	expect_error(debug.state(10))
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

# no data nodes
test_that("debug.state - no variables",
{	
	json <- system.file("testdata", "noVars.json", package = "provDebugR")
	
	provDebugR:::.clear()
	expect_warning(prov.debug.file(json))   # warning is due to deleted prov folder
	
	c2 <- utils::capture.output(c1 <- debug.state())
	
	expect_null(c1)
	expect_true(nchar(paste(c2, collapse='\n')) > 0)
})

# state at end:
# multiple scrips (with error), no error

# state at end of each script,
# state at beginning of each script

# helper functions to test
# .get.state.tables

# .get.closest.proc
# .get.last.var
# .get.state
# .get.output.state

# .get.state.tables
test_that("debug.state - .get.state.tables",
{
	# CASE: no data nodes
	#json <- system.file("testdata", "noDataNodes.json", package = "provDebugR")
	#provDebugR:::.clear()
	#expect_warning(prov.debug.file(json))   # warning is due to deleted prov folder

	#c1 <- provDebugR:::.get.state.tables()
	#expect_null(c1)
	
	# CASE: no variables
	json <- system.file("testdata", "noVars.json", package = "provDebugR")
	provDebugR:::.clear()
	expect_warning(prov.debug.file(json))   # warning is due to deleted prov folder

	c2 <- provDebugR:::.get.state.tables()
	expect_null(c2)
	
	# CASE: tests types of proc nodes to be removed
	# not an operation node, no edges, all edges removed.
	json <- system.file("testdata", "getStateTables.json", package = "provDebugR")
	provDebugR:::.clear()
	expect_warning(prov.debug.file(json))   # warning is due to deleted prov folder

	c3 <- provDebugR:::.get.state.tables()
	
	e3.proc.nodes <- data.frame(id = 'p4',
							   scriptNum = 1L,
							   startLine = 6L,
							   code = 'a <- 5',
							   stringsAsFactors = FALSE)
	e3.data.nodes <- data.frame(id = 'd1',
							   name = 'a',
							   value = '5',
							   fromEnv = FALSE,
							   stringsAsFactors = FALSE)
	e3.proc.data <- data.frame(id = 'pd1',
							   activity = 'p4',
							   entity = 'd1',
							   stringsAsFactors = FALSE)
	e3.data.proc <- data.frame(id = character(),
							   entity = character(),
							   activity = character(),
							   stringsAsFactors = FALSE)
	
	e3 <- list(e3.proc.nodes, e3.data.nodes, e3.proc.data, e3.data.proc)
	names(e3) <- c("proc.nodes", "data.nodes", "proc.data", "data.proc")
	
	expect_equivalent(c3, e3)
	
	# CASE: some nodes not removed as there are edges from variables
	json <- system.file("testdata", "fromEnv.json", package = "provDebugR")
	provDebugR:::.clear()
	expect_warning(prov.debug.file(json))   # warning is due to deleted prov folder
	
	c4 <- provDebugR:::.get.state.tables()
	
	e4.proc.nodes <- paste('p', c(3,4,5,6,7,8,9,10,12,13,14,15,16), sep='')
	e4.data.nodes <- paste('d', c(1,2,3,5,6,7,11,12,14,15), sep='')
	e4.proc.data <- paste('pd', c(1,2,4,5,6,10,11,13), sep='')
	e4.data.proc <- paste('dp', c(1,2,3,4,5,6,7,8,9,10,12,13,14,15,17,18), sep='')
	
	expect_equivalent(c4$proc.nodes$id, e4.proc.nodes)
	expect_equivalent(c4$data.nodes$id, e4.data.nodes)
	expect_equivalent(c4$proc.data$id, e4.proc.data)
	expect_equivalent(c4$data.proc$id, e4.data.proc)
	
	# skip test at beginning of test_that block
	skip(".get.state.tables - no data nodes")
})

# .get.valid.query.state
test_that("debug.state - .get.valid.query.state",
{
	json <- system.file("testdata", "exceptions.json", package = "provDebugR")
	provDebugR:::.clear()
	prov.debug.file(json)
	
	proc.nodes <- provDebugR:::.debug.env$proc.nodes
	
	# CASES
	c1 <- provDebugR:::.get.valid.query.state(            # 1 valid query
			proc.nodes, 3L, script.num = 1)
	c2 <- provDebugR:::.get.valid.query.state(            # multiple queries, with repeated
			proc.nodes, 3L, 5L, 3, script.num = 1)
	c3 <- provDebugR:::.get.valid.query.state(            # integer queries as different types
			proc.nodes, 3L, 5, "7", script.num = 1)
	c4 <- provDebugR:::.get.valid.query.state(            # different (valid) script number
			proc.nodes, 3L, 5, "7", script.num = 3)
	c5 <- provDebugR:::.get.valid.query.state(            # valid script number as a string
			proc.nodes, 3L, 5, script.num = "3")
	c6 <- provDebugR:::.get.valid.query.state(            # mix of valid and invalid queries
			proc.nodes, 3, 5.5, 7L, script.num = 3)
	
	c7 <- provDebugR:::.get.valid.query.state(            # no queries
			proc.nodes, script.num = 1)
	expect_warning(                                       # multiple script numbers
		c8 <- provDebugR:::.get.valid.query.state(
			proc.nodes, 3L, 5, "7", script.num = c(1,2)))
	expect_warning(                                       # script number is not an integer
		c9 <- provDebugR:::.get.valid.query.state(
			proc.nodes, 3L, 5, script.num = 2.5))
	expect_warning(                                       # script number is NA
		c10 <- provDebugR:::.get.valid.query.state(
			proc.nodes, 3L, 5, script.num = NA))
	
	c12 <- utils::capture.output(                         # invalid queries
		c11 <- provDebugR:::.get.valid.query.state(
			proc.nodes, "invalid", 5.5, TRUE, NA, script.num = 3))
	c14 <- utils::capture.output(                         # invalid script number
		c13 <- provDebugR:::.get.valid.query.state(
			proc.nodes, 5L , script.num = 10))
	
	# EXPECTED
	e1 <- data.frame(startLine = 3L, scriptNum = 1L)                     # 1 valid query
	e2 <- data.frame(startLine = c(3L,5L), scriptNum = c(1L,1L))         # multiple queries
	e3 <- data.frame(startLine = c(3L,5L,7L), scriptNum = c(1L,1L,1L))   # integer queries as different types
	e4 <- data.frame(startLine = c(3L,5L,7L), scriptNum = c(3L,3L,3L))   # different (valid) script number
	e5 <- data.frame(startLine = c(3L,5L), scriptNum = c(3L,3L))         # valid script number as a string
	e6 <- data.frame(startLine = c(3L,7L), scriptNum = c(3L,3L))         # mix of valid and invalid queries
	
	# TEST
	expect_equivalent(c1, e1)   # 1 valid query
	expect_equivalent(c2, e2)   # multiple queries
	expect_equivalent(c3, e3)   # integer queries as different types
	expect_equivalent(c4, e4)   # different (valid) script number
	expect_equivalent(c5, e5)   # valid script number as a string
	expect_equivalent(c6, e6)   # mix of valid and invalid queries
	
	expect_null(c7)        # no queries
	expect_null(c8)        # multiple script numbers
	expect_null(c9)        # script number is not an integer
	expect_null(c10)       # script number is NA
	
	expect_null(c11)                                      # invalid queries
	expect_true(nchar(paste(c12, collapse = '\n')) > 0)
	expect_null(c13)                                      # invalid script number
	expect_true(nchar(paste(c14, collapse = '\n')) > 0)
})
