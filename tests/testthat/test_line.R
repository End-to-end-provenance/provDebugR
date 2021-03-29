library(testthat)
library(provDebugR)

context("debug.line")

# === HELPER FUNCTIONS ======================================================= #

get.expected <- function()
{
	# SCRIPT 1 RESULTS
	# value column omitted from all data frames
	# p3
	r1.in <- NA 
	r1.out <- data.frame(name = "col1",
						 container = "vector",
						 dimension = "11",
						 type = "integer",
						 stringsAsFactors = FALSE)
	r1 <- list(input = r1.in, output = r1.out)
	
	script.1 <- list(`1` = r1)
	
	
	# SCRIPT 2 RESULTS
	# value column omitted from all data frames
	# p5
	r1.in <- data.frame(name = "col1",
						container = "vector",
						dimension = "11",
						type = "integer",
						stringsAsFactors = FALSE)
	r1.out <- data.frame(name = c("warning.msg","col1"),
						 container = c("vector","vector"),
						 dimension = c("1","10"),
						 type = c("character","integer"),
						 stringsAsFactors = FALSE)
	r1 <- list(input = r1.in, output = r1.out)
	
	# p6
	r2.in <- NA 
	r2.out <- data.frame(name = "warning.msg",
						 container = "vector",
						 dimension = "1",
						 type = "character",
						 stringsAsFactors = FALSE)
	r2 <- list(input = r2.in, output = r2.out)
	
	script.2 <- list(r1, r2)
	names(script.2) <- c(1:2)
	
	
	# SCRIPT 3 RESULTS
	# value column omitted from all data frames
	# p9
	r1.in <- NA 
	r1.out <- data.frame(name = "get.error.df",
						 container = as.character(NA),
						 dimension = as.character(NA),
						 type = "function",
						 stringsAsFactors = FALSE)
	r1 <- list(input = r1.in, output = r1.out)
	
	# p10
	r2.in <- data.frame(name = c("col1","get.error.df"),
						 container = c("vector",NA),
						 dimension = c("10",NA),
						 type = c("integer","function"),
						 stringsAsFactors = FALSE)
	r2.out <- data.frame(name = "df",
						 container = "data_frame",
						 dimension = "10,3",
						 type = "integer, character, integer",
						 stringsAsFactors = FALSE)
	r2 <- list(input = r2.in, output = r2.out)
	
	# p11
	r3.in <- data.frame(name = c("df","col1"),
						container = c("data_frame","vector"),
						dimension = c("10,3","10"),
						type = c("integer, character, integer","integer"),
						stringsAsFactors = FALSE)
	r3.out <- data.frame(name = "error.msg",
						 container = "vector",
						 dimension = "1",
						 type = "character",
						 stringsAsFactors = FALSE)
	r3 <- list(input = r3.in, output = r3.out)
	
	script.3 <- list(r1, r2, r3)
	names(script.3) <- c(1:3)
	
	
	# COMBINE
	expected <- list(script.1, script.2, script.3)
	names(expected) <- c("script.1", "script.2", "script.3")
	
	return(expected)
}

# === THE TESTS ============================================================== #

# no provenance
test_that("debug.line - no/empty provenance", 
{
	# clean debug environment of provDebugR first to ensure inital state
	provDebugR:::.clear()
	
	# initialisation not run
	expect_false(provDebugR:::.debug.env$has.graph)
	expect_error(debug.line(5))
	
	# empty provenance
	c0 <- system.file("testdata", "empty.json", package = "provDebugR")
	expect_error(prov.debug.file(c0))
	expect_false(provDebugR:::.debug.env$has.graph)
	expect_error(debug.line(5))
})

# Test case for debug.line, .get.valid.query.line
json <- system.file("testdata", "exceptions.json", package = "provDebugR")

provDebugR:::.clear()
expect_warning(prov.debug.file(json))   # warning due to deleted prov folder

expected <- get.expected()

# debug.line - all = TRUE
test_that("debug.line - all == TRUE",
{
	# CASES
	c2 <- utils::capture.output(     
		c1 <- debug.line(all = TRUE))
	c4 <- utils::capture.output(                             # with params
		c3 <- debug.line("8", script.num = 1, all = TRUE))
	
	c1$`1`$output <- c1$`1`$output[ , -2]   # omit value columns from testing
	c1$`2`$input <- c1$`2`$input[ , -2]
	c1$`2`$output <- c1$`2`$output[ , -2]
	c1$`3`$output <- c1$`3`$output[ , -2]
	c1$`4`$output <- c1$`4`$output[ , -2]
	c1$`5`$input <- c1$`5`$input[ , -2]
	c1$`5`$output <- c1$`5`$output[ , -2]
	c1$`6`$input <- c1$`6`$input[ , -2]
	c1$`6`$output <- c1$`6`$output[ , -2]
	
	c3$`1`$output <- c3$`1`$output[ , -2]   # omit value columns from testing
	c3$`2`$input <- c3$`2`$input[ , -2]
	c3$`2`$output <- c3$`2`$output[ , -2]
	c3$`3`$output <- c3$`3`$output[ , -2]
	c3$`4`$output <- c3$`4`$output[ , -2]
	c3$`5`$input <- c3$`5`$input[ , -2]
	c3$`5`$output <- c3$`5`$output[ , -2]
	c3$`6`$input <- c3$`6`$input[ , -2]
	c3$`6`$output <- c3$`6`$output[ , -2]
	
	
	# EXPECTED
	expected <- unlist(expected, recursive = FALSE)
	names(expected) <- c(1:6)
	
	
	# TEST
	expect_equivalent(c1, expected)
	expect_equivalent(c3, expected)
	
	expect_true(nchar(paste(c2, collapse='\n')) > 0)
	expect_true(nchar(paste(c4, collapse='\n')) > 0)
})

# debug.line - line queries
test_that("debug.line - line number queries",
{
	# CASES
	c2 <- utils::capture.output(                       # valid queries
		c1 <- debug.line(8, script.num = 3)[[1]])
	c4 <- utils::capture.output(
		c3 <- debug.line("8", script.num = 3)[[1]])
	
	c6 <- utils::capture.output(                       # repeated queries
		c5 <- debug.line("8",8,1, script.num = 3))
	
	c8 <- utils::capture.output(                       # valid & invalid queries
		c7 <- debug.line(3,-1, script.num = 1)[[1]])
	
	c10 <- utils::capture.output(                      # no input or output nodes
		c9 <- debug.line(1, script.num = 1))
	
	c12 <- utils::capture.output(                      # invalid lines
		c11 <- debug.line(20,-4, script.num = 1))
	
	c14 <- utils::capture.output(                      # no line
		c13 <- debug.line())
	
	# omit code columns from comparison
	c1$input <- c1$input[ ,-2]             # valid queries
	c1$output <- c1$output[ ,-2]
	c3$input <- c3$input[ ,-2]
	c3$output <- c3$output[ ,-2]
	
	c5$`1`$input <- c5$`1`$input[ ,-2]     # repeated queries
	c5$`1`$output <- c5$`1`$output[ ,-2]
	c5$`2`$output <- c5$`2`$output[ ,-2]
	names(c5) <- c(1:2)
	
	c7$output <- c7$output[ ,-2]           # valid & invalid queries
	
	
	# EXPECTED
	e1 <- expected$script.3$`2`       # valid queries
	e3 <- e1
	e5 <- expected$script.3[c(2,1)]   # repeated queries
	e7 <- expected$script.1$`1`       # valid & invalid queries
	
	
	# TEST
	expect_equivalent(c1, e1)   # valid queries
	expect_equivalent(c3, e3)
	expect_equivalent(c5, e5)   # repeated queries
	expect_equivalent(c7, e7)   # valid & invalid queries
	expect_null(c9)             # no input or output nodes
	expect_null(c11)            # invalid lines
	expect_null(c13)            # no line
	
	expect_true(nchar(paste(c2, collapse='\n')) > 0)    # valid queries
	expect_true(nchar(paste(c4, collapse='\n')) > 0)
	expect_true(nchar(paste(c6, collapse='\n')) > 0)    # repeated queries
	expect_true(nchar(paste(c8, collapse='\n')) > 0)    # valid & invalid queries
	expect_true(nchar(paste(c10, collapse='\n')) > 0)   # no input or output nodes
	expect_true(nchar(paste(c12, collapse='\n')) > 0)   # invalid lines
	expect_true(nchar(paste(c14, collapse='\n')) > 0)   # no line
})

# debug.line - script number queries
test_that("debug.line - script number queries",
{
	# CASES
	c2 <- utils::capture.output(                      # script.num = all
		c1 <- debug.line(1, script.num = "all"))
	c4 <- utils::capture.output(                      # multiple non int script nums
		c3 <- debug.line(1, script.num = as.character(c(2,3))))
	c6 <- utils::capture.output(                      # valid & invalid
		c5 <- debug.line(1, script.num = c(2,3,5)))
	c8 <- utils::capture.output(                      # invalid
		c7 <- debug.line(1, script.num = "2.5"))
	
	# omit code column from comparison
	c1$`1`$input <- c1$`1`$input[ ,-2]     # script.num = all
	c1$`1`$output <- c1$`1`$output[ ,-2]
	c1$`2`$output <- c1$`2`$output[ ,-2]
	
	c3$`1`$input <- c3$`1`$input[ ,-2]     # multiple non int script nums
	c3$`1`$output <- c3$`1`$output[ ,-2]
	c3$`2`$output <- c3$`2`$output[ ,-2]
	
	c5$`1`$input <- c5$`1`$input[ ,-2]     # valid & invalid
	c5$`1`$output <- c5$`1`$output[ ,-2]
	c5$`2`$output <- c5$`2`$output[ ,-2]
	
	
	# EXPECTED
	expected <- unlist(expected, recursive = FALSE)
	names(expected) <- c(1:6)
	
	e1 <- expected[c(2,4)]   # script.num = all
	names(e1) <- c(1,2)
	
	e3 <- e1                 # multiple non int script nums
	e5 <- e1                 # valid & invalid
	
	
	# TEST
	expect_equivalent(c1, e1)   # script.num = all
	expect_equivalent(c3, e3)   # multiple non int script nums
	expect_equivalent(c5, e5)   # valid & invalid
	expect_null(c7)             # invalid
	
	expect_true(nchar(paste(c2, collapse='\n')) > 0)   # script.num = all
	expect_true(nchar(paste(c4, collapse='\n')) > 0)   # multiple non int script nums
	expect_true(nchar(paste(c6, collapse='\n')) > 0)   # valid & invalid
	expect_true(nchar(paste(c8, collapse='\n')) > 0)   # invalid
})

# === TESTING SHARED FUNCTIONS =============================================== #

# .get.pos.line
test_that(".get.pos.line",
{
	proc.nodes <- provDebugR:::.debug.env$proc.nodes
	
	# CASES
	c1 <- provDebugR:::.get.pos.line(proc.nodes[0, ])              # no proc nodes
	c2 <- provDebugR:::.get.pos.line(proc.nodes[1, ])              # no operation nodes
	c3 <- provDebugR:::.get.pos.line(proc.nodes[c(1,3,11,13), ])   # with start and finish nodes
	
	c3 <- c3[ ,-5]   # omit code column from testing
	
	# EXPECTED
	# cols: p.id, startLine, scriptNum, scriptName, code
	e3 <- data.frame(p.id = c("p3", "p11"),
					 startLine = as.integer(c(3,10)),
					 scriptNum = as.integer(c(1,3)),
					 scriptName = c("exceptions.R","source_error.r"),
					 stringsAsFactors = FALSE)
	
	# TEST
	expect_null(c1)
	expect_null(c2)
	expect_equivalent(c3, e3)
})

# .get.query.line
test_that(".get.query.line",
{
	# CASES
	c1 <- provDebugR:::.get.query.line(all = TRUE)                   # all
	c2 <- provDebugR:::.get.query.line(1, all = TRUE)
	
	c3 <- provDebugR:::.get.query.line()                             # no queries
	
	c4 <- provDebugR:::.get.query.line(2,4)                          # line queries
	c5 <- provDebugR:::.get.query.line(7,-5, script.num = 5)
	
	c6 <- provDebugR:::.get.query.line(2,4, script.num = "all")      # script.num = "all"
	
	c7 <- provDebugR:::.get.query.line(2,2,4, script.num = c(5,2))   # multiple script nums
	
	
	# EXPECTED
	e1 <- data.frame(startLine = as.integer(c(1,3,1,6,1,8,10)),    # all
					 scriptNum = as.integer(c(1,1,2,2,3,3,3)),
					 stringsAsFactors = FALSE)
	e2 <- e1
	
	e4 <- data.frame(startLine = as.integer(c(2,4)),               # line queries
					 scriptNum = as.integer(c(1,1)),
					 stringsAsFactors = FALSE)
	e5 <- data.frame(startLine = as.integer(c(7,-5)),
					 scriptNum = as.integer(c(5,5)),
					 stringsAsFactors = FALSE)
	
	e6 <- data.frame(startLine = as.integer(c(2,4,2,4,2,4)),       # script.num = "all"
					 scriptNum = as.integer(c(1,1,2,2,3,3)),
					 stringsAsFactors = FALSE)
	
	e7 <- data.frame(startLine = as.integer(c(2,4,2,4)),           # multiple script nums
					 scriptNum = as.integer(c(5,5,2,2)),
					 stringsAsFactors = FALSE)
	
	
	# TEST
	expect_equivalent(c1,e1)   # all
	expect_equivalent(c2,e2)
	
	expect_null(c3)            # no queries
	
	expect_equivalent(c4,e4)   # line queries
	expect_equivalent(c5,e5)
	
	expect_equivalent(c6,e6)   # script.num = "all"
	
	expect_equivalent(c7,e7)   # multiple script nums
})

# .get.valid.query.line
test_that(".get.valid.query.line",
{
	# Get pos.nodes
	# cols: p.id, startLine, scriptNum, scriptName, code
	pos.nodes <- provDebugR:::.get.pos.line(provDebugR:::.debug.env$proc.nodes)
	
	# QUERIES
	q2 <- data.frame(startLine = as.integer(c(-4,1)),        # no valid queries
					 scriptNum = as.integer(c(1,5)),
					 stringsAsFactors = FALSE)
	
	q3 <- data.frame(startLine = as.character(c(3,10)),      # valid queries
					 scriptNum = as.character(c(1,3)),
					 stringsAsFactors = FALSE)
	q4 <- data.frame(startLine = as.integer(c(3,10)),
					 scriptNum = as.integer(c(1,3)),
					 stringsAsFactors = FALSE)
	
	q5 <- data.frame(startLine = as.character(c(3,3,10)),    # repeated queries
					 scriptNum = as.character(c(1,1,3)),
					 stringsAsFactors = FALSE)
	
	q6 <- data.frame(startLine = as.integer(c(6,20,1,10)),   # valid and invalid
					  scriptNum = as.integer(c(2,1,1,6)),
					  stringsAsFactors = FALSE)
	
	# CASES                                 
	c1 <- provDebugR:::.get.valid.query.line(pos.nodes, NULL)   # no queries                               
	c2 <- provDebugR:::.get.valid.query.line(pos.nodes, q2)     # no vaid queries
	c3 <- provDebugR:::.get.valid.query.line(pos.nodes, q3)     # valid queries
	c4 <- provDebugR:::.get.valid.query.line(pos.nodes, q4)
	c5 <- provDebugR:::.get.valid.query.line(pos.nodes, q5)     # repeated queries
	c6 <- provDebugR:::.get.valid.query.line(pos.nodes, q6)     # valid and invalid
	
	# EXPECTED
	e3 <- pos.nodes[c(2,7), ]   # valid queries
	e4 <- e3
	e5 <- e3                    # repeated queries
	e6 <- pos.nodes[c(4,1), ]   # valid and invalid
	
	# TEST
	expect_null(c1)            # no queries
	expect_null(c2)            # no valid queries
	expect_equivalent(c3,e3)   # valid queries
	expect_equivalent(c4,e4)
	expect_equivalent(c5,e5)   # repeated queries
	expect_equivalent(c6,e6)   # valid and invalid
})
