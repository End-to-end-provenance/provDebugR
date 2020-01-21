library(testthat)
library(provDebugR)

context("debug.lineage")

# === HELPER FUNCTIONS ======================================================= #

# helper function to get expected output for backward lineages
# uses test case: fromEnv
get.expected.backward <- function()
{
	# no backwards lineage for a and b (fromEnv variales)
	
	# index 1: d
	d <- data.frame(scriptNum = 1L,
					startLine = 1L,
					code = 'd <- 10',
					stringsAsFactors = FALSE)
	
	# index 2: vector.1
	vector.1 <- data.frame(scriptNum = as.integer(c(1,1)),
						   startLine = as.integer(c(1,2)),
						   code = c('d <- 10', 'vector.1 <- c(a:d)'),
						   stringsAsFactors = FALSE)
	
	# index 3: output
	output <- data.frame(scriptNum = 1L,
						 startLine = 24L,
						 code = 'print("End of script!")',
						 stringsAsFactors = FALSE)
	
	# index 4: e
	e <- data.frame(scriptNum = 1L,
					startLine = 5L,
					code = 'e <- a + 10',
					stringsAsFactors = FALSE)
	
	# index 5: f
	f <- data.frame(scriptNum = as.integer(c(1,1)),
					startLine = as.integer(c(1,6)),
					code = c('d <- 10', 'f <- d + 10'),
					stringsAsFactors = FALSE)
	
	# index 6: vector.2
	# omit code column
	vector.2 <- data.frame(scriptNum = rep(1L,4),
						   startLine = as.integer(c(1,5,6,7)),
						   stringsAsFactors = FALSE)
	
	# index 7: dev.2
	# omit code column
	dev.2 <- data.frame(scriptNum = rep(1L,10),
						startLine = as.integer(c(1,2,5,6,7,11,12,15,16,17)),
						stringsAsFactors = FALSE)
	
	# index 8: vector.3
	# omit code column
	vector.3 <- data.frame(scriptNum = rep(1L,3),
						   startLine = as.integer(c(1,2,15)),
						   stringsAsFactors = FALSE)
	
	# index 9: vector.4
	# omit code column
	vector.4 <- data.frame(scriptNum = rep(1L,5),
						   startLine = as.integer(c(1,5,6,7,16)),
						   stringsAsFactors = FALSE)
	
	# index 10: g
	g <- data.frame(scriptNum = 1L,
					startLine = 20L,
					code = 'g <- b + 50',
					stringsAsFactors = FALSE)
	
	# combine into list
	expected <- list(d, vector.1, output, e, f, vector.2, 
					 dev.2, vector.3, vector.4, g)
	names(expected) <- c("d", "vector.1", "output", "e", "f", "vector.2",
						 "dev.2", "vector.3", "vector.4", "g")
	
	return(expected)
}

# helper function to get expected output for forward lineages
# uses test case: fromEnv
get.expected.forward <- function()
{
	# no lineage for output
	
	# index 1: d
	# omit code column
	d <- data.frame(scriptNum = rep(1L,11),
					startLine = as.integer(c(1,2,3,6,7,8,11,12,15,16,17)),
					stringsAsFactors = FALSE)
	
	# index 2: a
	# omit code column
	a <- data.frame(scriptNum = rep(1L,10),
					startLine = as.integer(c(2,3,5,7,8,11,12,15,16,17)),
					stringsAsFactors = FALSE)
	
	# index 3: vector.1
	# omit code column
	vector.1 <- data.frame(scriptNum = rep(1L,6),
						   startLine = as.integer(c(2,3,11,12,15,17)),
						   stringsAsFactors = FALSE)
	
	# index 4: output
	output <- data.frame(scriptNum = 1L,
						 startLine = 3L,
						 code = 'print(vector.1)',
						 stringsAsFactors = FALSE)
	
	# index 5: e
	# omit code column
	e <- data.frame(scriptNum = rep(1L,7),
					startLine = as.integer(c(5,7,8,11,12,16,17)),
					stringsAsFactors = FALSE)
	
	# index 6: f
	# omit code column
	f <- data.frame(scriptNum = rep(1L,7),
					startLine = as.integer(c(6,7,8,11,12,16,17)),
					stringsAsFactors = FALSE)
	
	# index 7: vector.2
	# omit code column
	vector.2 <- data.frame(scriptNum = rep(1L,6),
						   startLine = as.integer(c(7,8,11,12,16,17)),
						   stringsAsFactors = FALSE)
	
	# index 8: dev.2
	# omit code column
	dev.2 <- data.frame(scriptNum = rep(1L,3),
						startLine = as.integer(c(11,12,17)),
						stringsAsFactors = FALSE)
	
	# index 9: vector.3
	# omit code column
	vector.3 <- data.frame(scriptNum = rep(1L,2),
						   startLine = as.integer(c(15,17)),
						   stringsAsFactors = FALSE)
	
	# index 10: vector.4
	# omit code column
	vector.4 <- data.frame(scriptNum = rep(1L,2),
						   startLine = as.integer(c(16,17)),
						   stringsAsFactors = FALSE)
	
	# index 11: b
	b <- data.frame(scriptNum = as.integer(c(1,1)),
					startLine = as.integer(c(20,21)),
					code = c('g <- b + 50', 'print(g)'),
					stringsAsFactors = FALSE)
	
	# index 12: g
	# identical to b
	g <- data.frame(scriptNum = as.integer(c(1,1)),
					startLine = as.integer(c(20,21)),
					code = c('g <- b + 50', 'print(g)'),
					stringsAsFactors = FALSE)
	
	# combine into list
	expected <- list(d, a, vector.1, output, e, f, vector.2, 
					 dev.2, vector.3, vector.4, b, g)
	names(expected) <- c("d", "a", "vector.1", "output", "e", "f", "vector.2", 
						 "dev.2", "vector.3", "vector.4", "b", "g")
	
	return(expected)
}

# === THE TESTS ============================================================== #

# no provenance
test_that("debug.lineage - no/empty provenance", 
{
	# clean debug environment of provDebugR first to ensure inital state
	provDebugR:::.clear()
	
	# initialisation not run
	expect_false(provDebugR:::.debug.env$has.graph)
	expect_error(provDebugR::debug.lineage("x"))
	
	# empty provenance
	c0 <- system.file("testdata", "empty.json", package = "provDebugR")
	expect_error(provDebugR::prov.debug.file(c0))
	expect_false(provDebugR:::.debug.env$has.graph)
	expect_error(provDebugR::debug.lineage("x"))
})

# no data nodes - is it possible to get a json like that?

# debug.lineage tests
json <- system.file("testdata", "fromEnv.json", package = "provDebugR")

provDebugR:::.clear()
expect_warning(prov.debug.file(json))   # warning is due to deleted prov folder

e.backward <- get.expected.backward()
e.forward <- get.expected.forward()

# debug.lineage - all (backward)
test_that("debug.lineage - all (backward)",
{
	# all
	c2 <- utils::capture.output(c1 <- debug.lineage(all = TRUE))
	c1$`vector.2` <- c1$`vector.2`[ , -3]   # omit code columns from test
	c1$`dev.2` <- c1$`dev.2`[ , -3]
	c1$`vector.3` <- c1$`vector.3`[ , -3]
	c1$`vector.4` <- c1$`vector.4`[ , -3]
	
	expect_equivalent(c1, e.backward)
	expect_true(nchar(paste(c2, collapse='\n')) > 0)
	
	# with a name queries (no lineage, valid, invalid)
	c4 <- utils::capture.output(c3 <- debug.lineage("a", "d", "invalid", all = TRUE))
	c3$`vector.2` <- c3$`vector.2`[ , -3]   # omit code columns from test
	c3$`dev.2` <- c3$`dev.2`[ , -3]
	c3$`vector.3` <- c3$`vector.3`[ , -3]
	c3$`vector.4` <- c3$`vector.4`[ , -3]
	
	expect_equivalent(c3, e.backward)
	expect_true(nchar(paste(c4, collapse='\n')) > 0)
	
	# start line query (valid)
	c5 <- debug.lineage(start.line = 11, all = TRUE)[[1]]
	c5 <- c5[ , -3]   # omit code column
	
	e5 <- e.backward$`dev.2`[c(1:6), ]
	
	expect_equivalent(c5, e5)
	
	# start line query (invalid)
	c7 <- utils::capture.output(c6 <- debug.lineage(start.line = 100, all = TRUE))
	
	expect_null(c6)
	expect_true(nchar(paste(c7, collapse='\n')) > 0)
	
	# start line query (multiple)
	expect_warning(
		c9 <- utils::capture.output(
			c8 <- debug.lineage(start.line = c(1,2), all = TRUE)))
	
	expect_null(c8)
	expect_true(nchar(paste(c9, collapse='\n')) > 0)
	
	# script num query (valid)
	c11 <- utils::capture.output(c10 <- debug.lineage(script.num = 1, all = TRUE))
	c10$`vector.2` <- c10$`vector.2`[ , -3]   # omit code columns from test
	c10$`dev.2` <- c10$`dev.2`[ , -3]
	c10$`vector.3` <- c10$`vector.3`[ , -3]
	c10$`vector.4` <- c10$`vector.4`[ , -3]
	
	expect_equivalent(c10, e.backward)
	expect_true(nchar(paste(c11, collapse='\n')) > 0)
	
	# script num query (invalid)
	c13 <- utils::capture.output(c12 <- debug.lineage(script.num = 100, all = TRUE))
	
	expect_null(c12)
	expect_true(nchar(paste(c13, collapse='\n')) > 0)
	
	# script num query (multiple)
	expect_warning(
		c15 <- utils::capture.output(
			c14 <- debug.lineage(start.line = c(1,2), all = TRUE)))
	
	expect_null(c14)
	expect_true(nchar(paste(c15, collapse='\n')) > 0)
})

# debug.lineage - all (forward)
test_that("debug.lineage - all (forward)",
{
	# all
	c1 <- debug.lineage(all = TRUE, forward = TRUE)
	
	c1$a <- c1$a[ , -3]   # omit code columns from testing
	c1$d <- c1$d[ , -3]
	c1$e <- c1$e[ , -3]
	c1$f <- c1$f[ , -3]
	c1$`dev.2` <- c1$`dev.2`[ , -3]
	c1$`vector.1` <- c1$`vector.1`[ , -3]
	c1$`vector.2` <- c1$`vector.2`[ , -3]
	c1$`vector.3` <- c1$`vector.3`[ , -3]
	c1$`vector.4` <- c1$`vector.4`[ , -3]
	
	expect_equivalent(c1, e.forward)
	
	# with a name queries (no lineage, valid, invalid)
	c2 <- debug.lineage("output", "a", "invalid", all = TRUE, forward = TRUE)
	
	c2$a <- c2$a[ , -3]   # omit code columns from testing
	c2$d <- c2$d[ , -3]
	c2$e <- c2$e[ , -3]
	c2$f <- c2$f[ , -3]
	c2$`dev.2` <- c2$`dev.2`[ , -3]
	c2$`vector.1` <- c2$`vector.1`[ , -3]
	c2$`vector.2` <- c2$`vector.2`[ , -3]
	c2$`vector.3` <- c2$`vector.3`[ , -3]
	c2$`vector.4` <- c2$`vector.4`[ , -3]
	
	expect_equivalent(c2, e.forward)
	
	# start line query (valid)
	c3 <- debug.lineage(start.line = 16, all = TRUE, forward = TRUE)[[1]]
	c3 <- c3[ , -3]   # omit code column from testing
	
	e3 <- e.forward$`vector.4`
	
	expect_equivalent(c3, e3)
	
	# start line query (invalid)
	c5 <- utils::capture.output(
		c4 <- debug.lineage(start.line = 100, all = TRUE, forward = TRUE))
	
	expect_null(c4)
	expect_true(nchar(paste(c5, collapse='\n')) > 0)
	
	# start line query (multiple)
	expect_warning(
		c7 <- utils::capture.output(
			c6 <- debug.lineage(start.line = c(1:2), all = TRUE, forward = TRUE)))
	
	expect_null(c6)
	expect_true(nchar(paste(c7, collapse='\n')) > 0)
	
	# script num query (valid)
	c8 <- debug.lineage(script.num = 1, all = TRUE, forward = TRUE)
	
	c8$a <- c8$a[ , -3]   # omit code columns from testing
	c8$d <- c8$d[ , -3]
	c8$e <- c8$e[ , -3]
	c8$f <- c8$f[ , -3]
	c8$`dev.2` <- c8$`dev.2`[ , -3]
	c8$`vector.1` <- c8$`vector.1`[ , -3]
	c8$`vector.2` <- c8$`vector.2`[ , -3]
	c8$`vector.3` <- c8$`vector.3`[ , -3]
	c8$`vector.4` <- c8$`vector.4`[ , -3]
	
	expect_equivalent(c8, e.forward)
	
	# script num query (invalid)
	c10 <- utils::capture.output(
		c9 <- debug.lineage(script.num = 100, all = TRUE, forward = TRUE))
	
	expect_null(c9)
	expect_true(nchar(paste(c10, collapse='\n')) > 0)
	
	# script num query (multiple)
	expect_warning(
		c12 <- utils::capture.output(
			c11 <- debug.lineage(script.num = c(1:2), all = TRUE, forward = TRUE)))
	
	expect_null(c11)
	expect_true(nchar(paste(c12, collapse='\n')) > 0)
})

# backwards lineage - name queries (valid and invalid)
# forwards lineage - name queries (valid and invalid)

# backwards lineage - start line queries (valid and invalid)
# forwards lineage - start line queries (valid and invalid)

# scriptNum cases

# no lineage cases (var)



# helper function tests
# get lineage tests (esp forward tests)
# output test - for removed last row for no dev.off case