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
	names(expected) <- c(1:3)
	
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
prov.debug.file(json)

expected <- get.expected()

# debug.line - all = TRUE
test_that("debug.line - all == TRUE",
{
	# scripts 1-3
	c2 <- utils::capture.output(
		c1 <- debug.line(all = TRUE, script.num = 1))
	c4 <- utils::capture.output(
		c3 <- debug.line(all = TRUE, script.num = 2))
	c6 <- utils::capture.output(
		c5 <- debug.line(all = TRUE, script.num = "3"))
	
	c1$`1`$output <- c1$`1`$output[ , -2]   # omit code columns from comparison
	
	c3$`1`$input <- c3$`1`$input[ , -2]
	c3$`1`$output <- c3$`1`$output[ , -2]
	c3$`2`$output <- c3$`2`$output[ , -2]
	
	c5$`1`$output <- c5$`1`$output[ , -2]
	c5$`2`$input <- c5$`2`$input[ , -2]
	c5$`2`$output <- c5$`2`$output[ , -2]
	c5$`3`$input <- c5$`3`$input[ , -2]
	c5$`3`$output <- c5$`3`$output[ , -2]
	
	e1 <- expected$`1`
	e3 <- expected$`2`
	e5 <- expected$`3`
	
	expect_equivalent(c1, e1)
	expect_equivalent(c3, e3)
	expect_equivalent(c5, e5)
	
	expect_true(nchar(paste(c2, collapse='\n')) > 0)
	expect_true(nchar(paste(c4, collapse='\n')) > 0)
	expect_true(nchar(paste(c5, collapse='\n')) > 0)
	
	# with valid line number queries
	c8 <- utils::capture.output(
		c7 <- debug.line(list(8,"10","8"), all = TRUE, script.num = 3))
	
	c7$`1`$output <- c7$`1`$output[ , -2]
	c7$`2`$input <- c7$`2`$input[ , -2]
	c7$`2`$output <- c7$`2`$output[ , -2]
	c7$`3`$input <- c7$`3`$input[ , -2]
	c7$`3`$output <- c7$`3`$output[ , -2]
	
	e7 <- expected$`3`
	
	expect_equivalent(c7, e7)
	expect_true(nchar(paste(c8, collapse='\n')) > 0)
	
	# with invalid line number queries
	c10 <- utils::capture.output(
		c9 <- debug.line(list("invalid.1", "invalid.2"), all = TRUE, script.num = 3))
	
	c9$`1`$output <- c9$`1`$output[ , -2]
	c9$`2`$input <- c9$`2`$input[ , -2]
	c9$`2`$output <- c9$`2`$output[ , -2]
	c9$`3`$input <- c9$`3`$input[ , -2]
	c9$`3`$output <- c9$`3`$output[ , -2]
	
	e9 <- expected$`3`
	
	expect_equivalent(c9, e9)
	expect_true(nchar(paste(c10, collapse='\n')) > 0)
	
	# with valid and invalid line number queries
	c12 <- utils::capture.output(
		c11 <- debug.line(list(8,"10","invalid"), all = TRUE, script.num = 3))
	
	c11$`1`$output <- c11$`1`$output[ , -2]
	c11$`2`$input <- c11$`2`$input[ , -2]
	c11$`2`$output <- c11$`2`$output[ , -2]
	c11$`3`$input <- c11$`3`$input[ , -2]
	c11$`3`$output <- c11$`3`$output[ , -2]
	
	e11 <- expected$`3`
	
	expect_equivalent(c11, e11)
	expect_true(nchar(paste(c12, collapse='\n')) > 0)
	
	# with invalid script num
	c12 <- utils::capture.output(
		c11 <- debug.line(all = TRUE, script.num = 10))
	
	expect_null(c11)
	expect_true(nchar(paste(c12, collapse='\n')) > 0)
	
	# with multiple script num
	expect_warning(
		c14 <- utils::capture.output(
			c13 <- debug.line(all = TRUE, script.num = c(1:2))))
	
	expect_null(c13)
	expect_true(nchar(paste(c14, collapse='\n')) > 0)
})

# debug.line - line queries
test_that("debug.line - line number queries",
{
	# single valid line
	c2 <- utils::capture.output(
		c1 <- debug.line(8, script.num = 3)[[1]])
	
	c1$input <- c1$input[ , -2]
	c1$output <- c1$output[ , -2]
	
	e1 <- expected$`3`[[2]]
	
	expect_equivalent(c1, e1)
	expect_true(nchar(paste(c2, collapse='\n')) > 0)
	
	# with NA as input
	c4 <- utils::capture.output(
		c3 <- debug.line(1, script.num = 3)[[1]])
	
	c3$output <- c3$output[ , -2]
	
	e3 <- expected$`3`[[1]]
	
	expect_equivalent(c3, e3)
	expect_true(nchar(paste(c4, collapse='\n')) > 0)
	
	# no input or output nodes
	c6 <- utils::capture.output(
		c5 <- debug.line(1, script.num = 1))
	
	expect_null(c5)
	expect_true(nchar(paste(c6, collapse='\n')) > 0)
	
	# valid line that is not a numeric
	c8 <- utils::capture.output(
		c7 <- debug.line("8", script.num = 3)[[1]])
	
	c7$input <- c7$input[ , -2]
	c7$output <- c7$output[ , -2]
	
	e7 <- expected$`3`[[2]]
	
	expect_equivalent(c7, e7)
	expect_true(nchar(paste(c8, collapse='\n')) > 0)
	
	# multiple valid lines, with repeat
	c10 <- utils::capture.output(
		c9 <- debug.line(c(8, "10", "8"), script.num = 3))
	
	c9$`1`$input <- c9$`1`$input[ , -2]
	c9$`1`$output <- c9$`1`$output[ , -2]
	c9$`2`$input <- c9$`2`$input[ , -2]
	c9$`2`$output <- c9$`2`$output[ , -2]
	
	e9 <- expected$`3`[c(2,3)]
	names(e9) <- c(1:2)
	
	expect_equivalent(c9, e9)
	expect_true(nchar(paste(c10, collapse='\n')) > 0)
	
	# invalid line
	c12 <- utils::capture.output(
		c11 <- debug.line(50, script.num = 1))
	
	expect_null(c11)
	expect_true(nchar(paste(c12, collapse='\n')) > 0)
	
	# multiple invalid lines
	c14 <- utils::capture.output(
		c13 <- debug.line(list("1.5", TRUE, 50), script.num = 1))
	
	expect_null(c13)
	expect_true(nchar(paste(c14, collapse='\n')) > 0)
	
	# some valid, some invalid lines
	c14 <- utils::capture.output(
		c13 <- debug.line(list("1.5", 1, "3"), script.num = 1)[[1]])
	
	c13$output <- c13$output[ , -2]
	
	e13 <- expected$`1`[[1]]
	
	expect_equivalent(c13, e13)
	expect_true(nchar(paste(c14, collapse='\n')) > 0)
})

# debug.line - script number queries
test_that("debug.line - script number queries",
{
	# valid script num
	c2 <- utils::capture.output(
		c1 <- debug.line(c(8,10), script.num = 3))
	
	c1$`1`$input <- c1$`1`$input[ , -2]
	c1$`1`$output <- c1$`1`$output[ , -2]
	c1$`2`$input <- c1$`2`$input[ , -2]
	c1$`2`$output <- c1$`2`$output[ , -2]
	
	e1 <- expected$`3`[c(2,3)]
	names(e1) <- c(1:2)
	
	expect_equivalent(c1, e1)
	expect_true(nchar(paste(c2, collapse='\n')) > 0)
	
	# script num is not a numeric
	c4 <- utils::capture.output(
		c3 <- debug.line(c(8,10), script.num = '3'))
	
	c3$`1`$input <- c3$`1`$input[ , -2]
	c3$`1`$output <- c3$`1`$output[ , -2]
	c3$`2`$input <- c3$`2`$input[ , -2]
	c3$`2`$output <- c3$`2`$output[ , -2]
	
	e3 <- expected$`3`[c(2,3)]
	names(e3) <- c(1:2)
	
	expect_equivalent(c3, e3)
	expect_true(nchar(paste(c4, collapse='\n')) > 0)
	
	# invalid script num
	c6 <- utils::capture.output(
		c5 <- debug.line(c(8,10), script.num = 5))
	
	expect_null(c5)
	expect_true(nchar(paste(c6, collapse='\n')) > 0)
	
	# multiple script numbers
	expect_warning(
		c8 <- utils::capture.output(
			c7 <- debug.line(c(1,3), script.num = c(1:2))))
	
	expect_null(c7)
	expect_true(nchar(paste(c8, collapse='\n')) > 0)
})

# .get.valid.query.line
test_that(".get.valid.query.line",
{
	# get pos.nodes
	pos.nodes <- provDebugR:::.debug.env$proc.nodes
	pos.nodes <- pos.nodes[pos.nodes$type == "Operation", ]
	pos.nodes <- pos.nodes[ , c("id", "startLine", "scriptNum", "name")]
	names(pos.nodes) <- c("id", "startLine", "scriptNum", "code")
	
	# valid line and script num
	c1 <- provDebugR:::.get.valid.query.line(pos.nodes, 3, 1)
	c1 <- c1[ , -4]   # omit code column from comparison
	
	e1 <- data.frame(id = "p3",
					 startLine = 3L,
					 scriptNum = 1L,
					 stringsAsFactors = FALSE)
	
	expect_equivalent(c1, e1)
	
	# invalid line
	c3 <- utils::capture.output(
		c2 <- provDebugR:::.get.valid.query.line(pos.nodes, 15, 1))
	
	expect_null(c2)
	expect_true(nchar(paste(c3, collapse='\n')) > 0)
	
	# multiple valid
	c4 <- provDebugR:::.get.valid.query.line(pos.nodes, list("3", 1), 1)
	c4 <- c4[ , -4]   # omit code column from comparison
	
	e4 <- data.frame(id = c("p3", "p2"),
					 startLine = as.integer(c(3,1)),
					 scriptNum = as.integer(c(1,1)),
					 stringsAsFactors = FALSE)
	
	expect_equivalent(c4, e4)
	
	# multiple invalid lines
	c6 <- utils::capture.output(
		c5 <- provDebugR:::.get.valid.query.line(pos.nodes, list(15, 1.2, TRUE), 1))
	
	expect_null(c5)
	expect_true(nchar(paste(c6, collapse='\n')) > 0)
	
	# multiple with valid and invalid
	c7 <- provDebugR:::.get.valid.query.line(pos.nodes, list("10", 15, 1.3 ,8), 3)
	c7 <- c7[ , -4]
	
	e7 <- data.frame(id = c("p11", "p10"),
					 startLine = as.integer(c(10,8)),
					 scriptNum = as.integer(c(3,3)),
					 stringsAsFactors = FALSE)
	
	expect_equivalent(c7, e7)
	
	# repeated line queries
	c8 <- provDebugR:::.get.valid.query.line(pos.nodes, list("10", 15, 8, 1.3 ,10), 3)
	c8 <- c8[ , -4]
	
	e8 <- data.frame(id = c("p11", "p10"),
					 startLine = as.integer(c(10,8)),
					 scriptNum = as.integer(c(3,3)),
					 stringsAsFactors = FALSE)
	
	expect_equivalent(c8, e8)
	
	# invalid script number
	c10 <- utils::capture.output(
		c9 <- provDebugR:::.get.valid.query.line(pos.nodes, c(1:3), 5))
	
	expect_null(c9)
	expect_true(nchar(paste(c10, collapse='\n')) > 0)
})
