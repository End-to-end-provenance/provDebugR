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
# .get.valid.query.state
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
