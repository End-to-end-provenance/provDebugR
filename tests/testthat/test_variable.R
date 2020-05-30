library(testthat)
library(provDebugR)

context("debug.variable")

# === HELPER FUNCTIONS ======================================================= #

# helper function to get the list of expected tables
# uses the test case `typeChanges.R`
get.expected <- function()
{
	# single assignment
	a <- data.frame(value = '1',
					container = 'vector',
					dimension = '1',
					type = 'integer',
					scriptNum = 1L,
					startLine = 2L,
					code = 'a <- 1L',
					stringsAsFactors = FALSE)
	
	# no type change
	cc <- data.frame(value = c('2','3'),
					 container = c('vector','vector'),
					 dimension = as.character(c(1,1)),
					 type = c('integer','integer'),
					 scriptNum = as.integer(c(1,1)),
					 startLine = as.integer(c(8,9)),
					 code = c('cc <- 2L','cc <- 3L'),
					 stringsAsFactors = FALSE)
	
	# container change
	d <- data.frame(value = c('4','4'),
					container = c('vector','list'),
					dimension = as.character(c(1,1)),
					type = c('numeric',NA),
					scriptNum = as.integer(c(1,1)),
					startLine = as.integer(c(12,13)),
					code = c('d <- 4','d <- as.list(d)'),
					stringsAsFactors = FALSE)
	
	# dimension change
	# omit value column
	e <- data.frame(container = c('matrix','matrix'),
					dimension = c('4,25','5,20'),
					type = c('integer','integer'),
					scriptNum = as.integer(c(1,1)),
					startLine = as.integer(c(16,17)),
					code = c('e <- matrix(c(1:100), 4)','e <- matrix(c(1:100), 5)'),
					stringsAsFactors = FALSE)
	
	# type change
	f <- data.frame(value = c('5','5'),
					container = c('vector','vector'),
					dimension = as.character(c(1,1)),
					type = c('numeric','integer'),
					scriptNum = as.integer(c(1,1)),
					startLine = as.integer(c(20,21)),
					code = c('f <- 5','f <- as.integer(f)'),
					stringsAsFactors = FALSE)
	
	# multiple type changes in sequence
	g <- data.frame(value = c('6','"six"','TRUE'),
					container = c('vector','vector','vector'),
					dimension = as.character(c(1,1,1)),
					type = c('numeric','character','logical'),
					scriptNum = as.integer(c(1,1,1)),
					startLine = as.integer(c(24,25,26)),
					code = c('g <- 6','g <- "six"','g <- TRUE'),
					stringsAsFactors = FALSE)
	
	# multiple type changes, with no type change
	value <- c('FALSE','TRUE','"seven"','"eight"','8','9')
	container <- rep('vector', 6)
	dimension <- rep('1', 6)
	type <- c('logical','logical','character','character','integer','integer')
	scriptNum <- rep(1L,6)
	startLine <- as.integer(c(29:34))
	code <- c('h <- FALSE', 'h <- TRUE', 'h <- "seven"',
			  'h <- "eight"', 'h <- 8L', 'h <- 9L')
	
	h <- data.frame(value, container, dimension, type,
					scriptNum, startLine, code, stringsAsFactors = FALSE)
	
	# special valTypes
	# null, environment, function, factor, posixct
	# omit value and code column
	container <- as.character(rep(NA,5))
	dimension <- as.character(rep(NA,5))
	type <- c('null', 'environment', 'function', 'factor', 'POSIXct')
	scriptNum <- rep(1L,5)
	startLine <- as.integer(c(38:42))
	
	s <- data.frame(container, dimension, type, scriptNum, startLine,
					stringsAsFactors = FALSE)
	
	# combine into list
	expected <- list(a,cc,d,e,f,g,h,s)
	names(expected) <- c("a","cc","d","e","f","g","h","s")
	
	return(expected)
}

# === THE TESTS ============================================================== #

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

# no data nodes
test_that("debug.variable - no data nodes",
{
	skip("debug.variable - no data nodes")
	
	json <- system.file("testdata", "noDataNodes.json", package = "provDebugR")
	
	provDebugR:::.clear()
	expect_warning(prov.debug.file(json))   # warning is due to deleted prov folder
	
	c2 <- utils::capture.output(c1 <- debug.variable(all = TRUE))
	
	expect_null(c1)
	expect_true(nchar(paste(c2, collapse='\n')) > 0)
})

# no variables
test_that("debug.variable - no variables",
{
	json <- system.file("testdata", "noVars.json", package = "provDebugR")
	
	provDebugR:::.clear()
	expect_warning(prov.debug.file(json))   # warning is due to deleted prov folder
	
	c2 <- utils::capture.output(c1 <- debug.variable(all = TRUE))
	
	expect_null(c1)
	expect_true(nchar(paste(c2, collapse='\n')) > 0)
})

# debug.variable tests
json <- system.file("testdata", "typeChanges.json", package = "provDebugR")

provDebugR:::.clear()
expect_warning(prov.debug.file(json))   # warning is due to deleted prov folder

expected <- get.expected()

# debug.variable - all
test_that("debug.variable (all == TRUE)",
{
	# no variables, default val.type and script.num
	c1 <- debug.variable(all = TRUE)
	c1$e <- c1$e[ ,-1]         # omit columns from test
	c1$s <- c1$s[ ,c(-1,-7)]
	
	expect_equivalent(c1, expected)
	
	# variables queried, default val.type and script.num
	c2 <- debug.variable("s", all = TRUE)
	c2$e <- c2$e[ ,-1]         # omit columns from test
	c2$s <- c2$s[ ,c(-1,-7)]
	
	expect_equivalent(c2, expected)
	
	# val.type query
	c3 <- debug.variable(val.type = "logical", all = TRUE)
	
	e3 <- expected[c(6,7)]
	e3$g <- e3$g[3, ]
	e3$h <- e3$h[c(1,2), ]
	
	expect_equivalent(c3, e3)
	
	# special val.type query
	c4 <- debug.variable(val.type = "environment", all = TRUE)[[1]]
	c4 <- c4[ ,c(-1,-7)]   # omit columns from test
	
	e4 <- expected$s
	e4 <- e4[2, ]
	
	expect_equivalent(c4, e4)
})

# debug.variable - no variable queries
test_that("debug.variable (no variable queries)",
{
	# CASES
	c2 <- utils::capture.output(c1 <- debug.variable())
	c4 <- utils::capture.output(c3 <- debug.variable(val.type = "integer"))
	c6 <- utils::capture.output(c5 <- debug.variable(script.num = 5))
	
	c2 <- paste(c2, collapse='\n')
	c4 <- paste(c4, collapse='\n')
	c6 <- paste(c6, collapse='\n')
	
	# TEST: returned value
	expect_null(c1)
	expect_null(c3)
	expect_null(c5)
	
	# TEST: output message
	expect_true(nchar(c2) > 0)
	expect_true(nchar(c4) > 0)
	expect_true(nchar(c6) > 0)
})

# debug.variable - variable name queries
test_that("debug.variable (variable name queries)",
{
	# all valid variable names
	c1 <- debug.variable("cc", "a")
	e1 <- expected[c(2,1)]
	
	expect_equivalent(c1,e1)
	
	# no data node with name
	c3 <- utils::capture.output(c2 <- debug.variable("invalid"))
	
	expect_null(c2)
	expect_true(nchar(paste(c3, collapse='\n')) > 0)
	
	# data node exists, but is not a variable
	c5 <- utils::capture.output(c4 <- debug.variable("dev.2"))
	
	expect_null(c4)
	expect_true(nchar(paste(c5, collapse='\n')) > 0)
	
	# some valid, some invalid variable names
	c6 <- debug.variable("a", "invalid", "g", "h", "dev.2")
	e6 <- expected[c(1,6,7)]
	
	expect_equivalent(c6,e6)
	
	# repeated queries
	c7 <- debug.variable("a", "a", "cc")
	e7 <- expected[c(1,2)]
	
	expect_equivalent(c7,e7)
})

# debug.variable - valType queries
test_that("debug.variable (val.type queries)",
{
	# CASE: Valid valType
	# variable does not have data node with queried valType
	c2 <- utils::capture.output(c1 <- debug.variable("a", val.type = "logical"))
	
	expect_null(c1)
	expect_true(nchar(paste(c2, collapse='\n')) > 0)
	
	# variables queried have data nodes with queried valType
	c3 <- debug.variable("h", "f", val.type = "integer")
	
	e3 <- expected[c(7,5)]
	e3$f <- e3$f[2, ]
	e3$h <- e3$h[c(5,6), ]
	
	expect_equivalent(c3,e3)
	
	# some variables queried have data nodes with queried valType
	c4 <- debug.variable("s", "f", "d", "h", val.type = "integer")
	
	e4 <- expected[c(5,7)]
	e4$f <- e4$f[2, ]
	e4$h <- e4$h[c(5,6), ]
	
	expect_equivalent(c4,e4)
	
	# CASE: Special valType
	c5 <- debug.variable("h","s", val.type = "function")[[1]]
	c5 <- c5[ , c(-1,-7)]   # omit columns
	
	e5 <- expected$s
	e5 <- e5[3, ]
	
	expect_equivalent(c5,e5)
	
	# CASE: invalid valType
	c7 <- utils::capture.output(c6 <- debug.variable(val.type = "language", all = TRUE))
	
	expect_null(c6)
	expect_true(nchar(paste(c7, collapse='\n')) > 0)
	
	# CASE: multiple valTypes queried
	expect_warning(
		c9 <- utils::capture.output(	
				c8 <- debug.variable("h", val.type = c("integer", "logical"))))
	
	expect_null(c6)
	expect_true(nchar(paste(c7, collapse='\n')) > 0)
})

# debug.variable - script queries
test_that("debug.variable (script.num queries)",
{
	# invalid script number
	c2 <- utils::capture.output(c1 <- debug.variable("a", script.num = 6))
	
	expect_null(c1)
	expect_true(nchar(paste(c2, collapse='\n')) > 0)
	
	# multiple script numbers
	expect_warning(
		c4 <- utils::capture.output(
			c3 <- debug.variable("a", script.num = c(7,8))))
	
	expect_null(c3)
	expect_true(nchar(paste(c4, collapse='\n')) > 0)
	
	# script number is not an integer
	expect_warning(
		c6 <- utils::capture.output(
			c5 <- debug.variable("a", script.num = 9.5)))
	
	expect_null(c5)
	expect_true(nchar(paste(c6, collapse='\n')) > 0)
})

# === TESTING SHARED FUNCTIONS =============================================== #
# --- functions shared with debug.lineage ---

# .get.pos.var - general case
test_that(".get.pos.var (valid)",
{
	# CASE: data nodes in different scripts
	# test for data nodes and variables
	json <- system.file("testdata", "exceptions.json", package = "provDebugR")
	
	provDebugR:::.clear()
	expect_warning(prov.debug.file(json))   # warning due to deleted prov folder
	
	c1 <- provDebugR:::.get.pos.var(provDebugR:::.debug.env$data.nodes)
	c2 <- provDebugR:::.get.pos.var(
				provDebugR:::.debug.env$data.nodes[
					provDebugR:::.debug.env$data.nodes$type == "Data" | 
					provDebugR:::.debug.env$data.nodes$type == "Snapshot", ])
	
	e1 <- system.file("testexpected", "posVar_exceptions_full.csv", package = "provDebugR")
	e2 <- system.file("testexpected", "posVar_exceptions_vars.csv", package = "provDebugR")
	
	e1 <- read.csv(e1, header = TRUE, row.names = 1, stringsAsFactors = FALSE)
	e2 <- read.csv(e2, header = TRUE, row.names = 1, stringsAsFactors = FALSE)
	
	expect_equivalent(c1, e1)
	expect_equivalent(c2, e2)
	
	
	# CASE: special valTypes
	# test for data nodes and variables
	json <- system.file("testdata", "typeChanges.json", package = "provDebugR")
	
	provDebugR:::.clear()
	expect_warning(prov.debug.file(json))   # warning due to deleted prov folder
	
	c3 <- provDebugR:::.get.pos.var(provDebugR:::.debug.env$data.nodes)
	c4 <- provDebugR:::.get.pos.var(
				provDebugR:::.debug.env$data.nodes[
					provDebugR:::.debug.env$data.nodes$type == "Data" | 
					provDebugR:::.debug.env$data.nodes$type == "Snapshot", ])
	
	e3 <- system.file("testexpected", "posVar_typeChanges_full.csv", package = "provDebugR")
	e4 <- system.file("testexpected", "posVar_typeChanges_vars.csv", package = "provDebugR")
	
	e3 <- read.csv(e3, header = TRUE, row.names = 1, stringsAsFactors = FALSE)
	e4 <- read.csv(e4, header = TRUE, row.names = 1, stringsAsFactors = FALSE)
	
	expect_equivalent(c3, e3)
	expect_equivalent(c4, e4)
})

# .get.pos.var - fromEnv variables
test_that(".get.pos.var (fromEnv variables)",
{
	# CASE: variables where fromEnv is TRUE
	# test for data nodes and variables
	json <- system.file("testdata", "fromEnv.json", package = "provDebugR")
	
	provDebugR:::.clear()
	expect_warning(prov.debug.file(json))   # warning from deleted prov folder
	
	c1 <- provDebugR:::.get.pos.var(provDebugR:::.debug.env$data.nodes)
	c2 <- provDebugR:::.get.pos.var(
				provDebugR:::.debug.env$data.nodes[
					provDebugR:::.debug.env$data.nodes$type == "Data" | 
					provDebugR:::.debug.env$data.nodes$type == "Snapshot", ])
	
	e1 <- system.file("testexpected", "posVar_fromEnv_full.csv", package = "provDebugR")
	e2 <- system.file("testexpected", "posVar_fromEnv_vars.csv", package = "provDebugR")
	
	e1 <- read.csv(e1, header = TRUE, row.names = 1, stringsAsFactors = FALSE)
	e2 <- read.csv(e2, header = TRUE, row.names = 1, stringsAsFactors = FALSE)
	
	expect_equivalent(c1, e1)
	expect_equivalent(c2, e2)
})

# .get.pos.var - has data nodes, no variables
test_that(".get.pos.var (no variables)", 
{
	json <- system.file("testdata", "noVars.json", package = "provDebugR")
	
	provDebugR:::.clear()
	expect_warning(prov.debug.file(json))   # warning due to deleted prov folder
	
	c1 <- provDebugR:::.get.pos.var(
				provDebugR:::.debug.env$data.nodes[
					provDebugR:::.debug.env$data.nodes$type == "Data" | 
					provDebugR:::.debug.env$data.nodes$type == "Snapshot", ])
	
	expect_null(c1)
})

# .get.query.var
test_that(".get.query.var",
{
	json <- system.file("testdata", "exceptions.json", package = "provDebugR")
	expect_warning(provDebugR::prov.debug.file(json))
	
	# no vars
	expect_null(provDebugR:::.get.query.var(NULL))
	
	# 1 object queried
	c1 <- provDebugR:::.get.query.var("col1")
	
	e1 <- data.frame(name = rep("col1", 6), 
					 valType = rep("all", 6),
					 startLine = c(3,1,3,1,3,1), 
					 scriptNum = c(1,1,2,2,3,3), 
					 stringsAsFactors = FALSE)
	
	expect_equivalent(c1, e1)
	
	# multiple queries
	c2 <- provDebugR:::.get.query.var(c("x","y","z"),
									  val.type = c(5,6),
									  start.line = c(3:4),
									  script.num = c(1:2))
	
	e2 <- data.frame(name = rep(c("x","y","z"), each = 8), 
					 valType =   rep(c(5,6,5,6,5,6,5,6), 3),
					 startLine = rep(c(3,3,4,4,3,3,4,4), 3),
					 scriptNum = rep(c(1,1,1,1,2,2,2,2), 3), 
					 stringsAsFactors = FALSE)
	
	expect_equivalent(c2, e2)
	
	# repeated queries
	c3 <- provDebugR:::.get.query.var(c("col1","df","df"), script.num = 1)
	
	e3 <- data.frame(name = c("col1","col1","df"), 
					 valType = c("all","all","all"),
					 startLine = c(3,1,8), 
					 scriptNum = c(1,1,1), 
					 stringsAsFactors = FALSE)
	
	expect_equivalent(c3, e3)
	
	# with invalid query
	c4 <- provDebugR:::.get.query.var(c("col1","4","5"), script.num = 1)
	
	e4 <- data.frame(name = c("col1","col1","4","5"), 
					 valType = c("all","all","all","all"),
					 startLine = c(3,1,NA,NA), 
					 scriptNum = c(1,1,1,1), 
					 stringsAsFactors = FALSE)
	
	expect_equivalent(c4, e4)
})

# .get.valid.query.var - all valid queries
test_that(".get.valid.query.var (all valid queries)",
{
	# POS.NODES
	# cols: d.id, p.id, name, valType, startLine, scriptNum
	p.full <- system.file("testexpected", "posVar_typeChanges_full.csv", package = "provDebugR")
	p.vars <- system.file("testexpected", "posVar_typeChanges_vars.csv", package = "provDebugR")
	
	p.full <- read.csv(p.full, header = TRUE, row.names = 1, stringsAsFactors = FALSE)
	p.vars <- read.csv(p.vars, header = TRUE, row.names = 1, stringsAsFactors = FALSE)
	
	# QUERIES
	# cols: name, valType, startLine, scriptNum
	q1 <- data.frame(name = 'a',                 # single data node for variable
					 valType = "all",
					 startLine = NA,
					 scriptNum = 1,
					 stringsAsFactors = FALSE)
	q2 <- data.frame(name = 's',                 # multiple datas node for variable
					 valType = "all",
					 startLine = NA,
					 scriptNum = 1,
					 stringsAsFactors = FALSE)
	q3 <- data.frame(name = c("f","d","g"),      # container query
					 valType = c('vector','vector','vector'), 
					 startLine = c(NA,NA,NA), 
					 scriptNum = c(1,1,1),
					 stringsAsFactors = FALSE)
	q4 <- data.frame(name = c("a","f","h"),      # type query
					 valType = c('integer','integer','integer'), 
					 startLine = c(NA,NA,NA), 
					 scriptNum = c(1,1,1),
					 stringsAsFactors = FALSE)
	q5 <- data.frame(name = 's',                 # special valType query
					 valType = 'environment',
					 startLine = NA,
					 scriptNum = 1,
					 stringsAsFactors = FALSE)
	q6 <- data.frame(name = 's',                 # line query is string, script num is string
					 valType = "all",
					 startLine = "40",
					 scriptNum = "1",
					 stringsAsFactors = FALSE)
	q7 <- data.frame(name = "dev.2",             # is data node, but not variable
					 valType = "all", 
					 startLine = NA, 
					 scriptNum = 1,
					 stringsAsFactors = FALSE)
	q8 <- data.frame(name = "Rplots.pdf",        # NA for startLine and scriptNum
					 valType = "all", 
					 startLine = as.integer(NA), 
					 scriptNum = 1,
					 stringsAsFactors = FALSE)
	
	# CASES
	c1 <- provDebugR:::.get.valid.query.var(p.full, q1, forward = FALSE)    # single data node for variable
	c2 <- provDebugR:::.get.valid.query.var(p.full, q1, forward = TRUE)
	c3 <- provDebugR:::.get.valid.query.var(p.vars, q1, forward = FALSE)
	c4 <- provDebugR:::.get.valid.query.var(p.vars, q1, forward = TRUE)
	
	c5 <- provDebugR:::.get.valid.query.var(p.full, q2, forward = FALSE)    # multiple datas node for variable
	c6 <- provDebugR:::.get.valid.query.var(p.full, q2, forward = TRUE)
	c7 <- provDebugR:::.get.valid.query.var(p.vars, q2, forward = FALSE)
	c8 <- provDebugR:::.get.valid.query.var(p.vars, q2, forward = TRUE)
	
	c9 <- provDebugR:::.get.valid.query.var(p.full, q3, forward = FALSE)    # container query
	c10 <- provDebugR:::.get.valid.query.var(p.full, q3, forward = TRUE)
	c11 <- provDebugR:::.get.valid.query.var(p.vars, q3, forward = FALSE)
	c12 <- provDebugR:::.get.valid.query.var(p.vars, q3, forward = TRUE)
	
	c13 <- provDebugR:::.get.valid.query.var(p.full, q4, forward = FALSE)   # type query
	c14 <- provDebugR:::.get.valid.query.var(p.full, q4, forward = TRUE)
	c15 <- provDebugR:::.get.valid.query.var(p.vars, q4, forward = FALSE)
	c16 <- provDebugR:::.get.valid.query.var(p.vars, q4, forward = TRUE)
	
	c17 <- provDebugR:::.get.valid.query.var(p.full, q5, forward = FALSE)   # special valType query
	c18 <- provDebugR:::.get.valid.query.var(p.full, q5, forward = TRUE)
	c19 <- provDebugR:::.get.valid.query.var(p.vars, q5, forward = FALSE)
	c20 <- provDebugR:::.get.valid.query.var(p.vars, q5, forward = TRUE)
	
	c21 <- provDebugR:::.get.valid.query.var(p.full, q6, forward = FALSE)   # line query is string, script num is string
	c22 <- provDebugR:::.get.valid.query.var(p.full, q6, forward = TRUE)
	c23 <- provDebugR:::.get.valid.query.var(p.vars, q6, forward = FALSE)
	c24 <- provDebugR:::.get.valid.query.var(p.vars, q6, forward = TRUE)
	
	c25 <- provDebugR:::.get.valid.query.var(p.full, q7, forward = FALSE)   # is data node, but not variable
	c26 <- provDebugR:::.get.valid.query.var(p.full, q7, forward = TRUE)
	
	c27 <- provDebugR:::.get.valid.query.var(p.full, q8, forward = FALSE)   # NA for startLine and scriptNum
	c28 <- provDebugR:::.get.valid.query.var(p.full, q8, forward = TRUE)
	
	# EXPECTED
	# cols: d.id, name, valType, startLine, scriptNum
	e1 <- cbind('d.id' = 'd1',                  # single data node for variable
				q1, stringsAsFactors = FALSE)
	
	e2 <- cbind('d.id' = 'd25',                 # single data node for variable
				q2, stringsAsFactors = FALSE)
	e3 <- cbind('d.id' = 'd21',
				q2, stringsAsFactors = FALSE)
	
	e4 <- cbind('d.id' = c('d11','d6','d14'),   # container query (f,d,g,vector)
				q3, stringsAsFactors = FALSE)
	e5 <- cbind('d.id' = c('d10','d6','d12'),
				q3, stringsAsFactors = FALSE)
	
	e6 <- cbind('d.id' = c('d1','d11','d20'),   # type query (a,f,h,integer)
				q4, stringsAsFactors = FALSE)
	e7 <- cbind('d.id' = c('d1','d11','d19'),
				q4, stringsAsFactors = FALSE)
	
	e8 <- cbind('d.id' = 'd22',                 # special valType query (s,environment)
				q5, stringsAsFactors = FALSE)
	
	e9 <- cbind('d.id' = 'd23',                 # line query is string, script num is string (40)
				q6, stringsAsFactors = FALSE)
	
	e10 <- cbind('d.id' = 'd4',                 # is data node, but not variable
				q7, stringsAsFactors = FALSE)
	e11 <- cbind('d.id' = 'd2',
				q7, stringsAsFactors = FALSE)
	
	e12 <- cbind('d.id' = 'd26',                # NA for startLine and scriptNum
				q8, stringsAsFactors = FALSE)
	
	# TEST
	expect_equivalent(c1,e1)     # single data node for variable
	expect_equivalent(c2,e1)
	expect_equivalent(c3,e1)
	expect_equivalent(c4,e1)
	
	expect_equivalent(c5,e2)     # multiple datas node for variable
	expect_equivalent(c6,e3)
	expect_equivalent(c7,e2)
	expect_equivalent(c8,e3)
	
	expect_equivalent(c9,e4)     # container query (f,d,g,vector)
	expect_equivalent(c10,e5)
	expect_equivalent(c11,e4)
	expect_equivalent(c12,e5)
	
	expect_equivalent(c13,e6)    # type query (a,f,h,integer)
	expect_equivalent(c14,e7)
	expect_equivalent(c15,e6)
	expect_equivalent(c16,e7)
	
	expect_equivalent(c17,e8)    # special valType query (s,environment)
	expect_equivalent(c18,e8)
	expect_equivalent(c19,e8)
	expect_equivalent(c20,e8)
	
	expect_equivalent(c21,e9)    # line query is string, script num is string (40)
	expect_equivalent(c22,e9)
	expect_equivalent(c23,e9)
	expect_equivalent(c24,e9)
	
	expect_equivalent(c25,e10)   # is data node, but not variable
	expect_equivalent(c26,e11)
	
	expect_equivalent(c27,e12)   # NA for startLine and scriptNum
	expect_equivalent(c28,e12)
})

# .get.valid.query.var - all invalid queries
test_that(".get.valid.query.var (all invalid queries)",
{
	# POS.NODES 
	# cols: d.id, p.id, name, valType, startLine, scriptNum
	p.full <- system.file("testexpected", "posVar_typeChanges_full.csv", package = "provDebugR")
	p.vars <- system.file("testexpected", "posVar_typeChanges_vars.csv", package = "provDebugR")
	
	p.full <- read.csv(p.full, header = TRUE, row.names = 1, stringsAsFactors = FALSE)
	p.vars <- read.csv(p.vars, header = TRUE, row.names = 1, stringsAsFactors = FALSE)
	
	# QUERIES 
	# cols: name, valType, startLine, scriptNum
	q1 <- NULL                                   # no query
	q2 <- data.frame("name" = "invalid",         # data node with name does not exist
					 "valType"= "all", 
					 "startLine" = "all", 
					 "scriptNum" = 1,
					 stringsAsFactors = FALSE)
	q3 <- data.frame("name" = "dev.2",           # is data node, but not variable
					 "valType"= "all", 
					 "startLine" = "all", 
					 "scriptNum" = 1,
					 stringsAsFactors = FALSE)
	q4 <- data.frame("name" = c("a","s"),        # invalid valType
					 "valType"= c("language","language"), 
					 "startLine" = c("all","all"), 
					 "scriptNum" = c(1,1),
					 stringsAsFactors = FALSE)
	q5 <- data.frame("name" = c("a","s"),        # invalid start lines
					 "valType"= c("all","all"), 
					 "startLine" = c(50,2), 
					 "scriptNum" = c(1,1),
					 stringsAsFactors = FALSE)
	q6 <- data.frame("name" = "a",               # invalid script num
					 "valType"= "all", 
					 "startLine" = "all", 
					 "scriptNum" = 5,
					 stringsAsFactors = FALSE)
	q7 <- data.frame("name" = "a",               # start line is not an integer
					 "valType"= "all", 
					 "startLine" = 1.5, 
					 "scriptNum" = 1,
					 stringsAsFactors = FALSE)
	q8 <- data.frame("name" = "a",               # script num is not an integer
					 "valType"= "all", 
					 "startLine" = "all", 
					 "scriptNum" = 1.5,
					 stringsAsFactors = FALSE)
	
	# CASES
	c1 <- provDebugR:::.get.valid.query.var(p.full, q1)         # no query
	c2 <- provDebugR:::.get.valid.query.var(p.vars, q1)
	
	c4 <- utils::capture.output(                          # data node with name does not exist
		c3 <- provDebugR:::.get.valid.query.var(p.full, q2))
	c6 <- utils::capture.output(
		c5 <- provDebugR:::.get.valid.query.var(p.vars, q2))
	
	c8 <- utils::capture.output(                          # is data node, but not variable
		c7 <- provDebugR:::.get.valid.query.var(p.vars, q3))
	
	c10 <- utils::capture.output(                         # invalid valType
		c9 <- provDebugR:::.get.valid.query.var(p.full, q4))
	c12 <- utils::capture.output(
		c11 <- provDebugR:::.get.valid.query.var(p.vars, q4))
	
	c14 <- utils::capture.output(                         # invalid start lines
		c13 <- provDebugR:::.get.valid.query.var(p.full, q5))
	c16 <- utils::capture.output(
		c15 <- provDebugR:::.get.valid.query.var(p.vars, q5))
	
	c18 <- utils::capture.output(                         # invalid script num
		c17 <- provDebugR:::.get.valid.query.var(p.full, q6))
	c20 <- utils::capture.output(
		c19 <- provDebugR:::.get.valid.query.var(p.vars, q6))
	
	c22 <- utils::capture.output(                         # start line is not an integer
		c21 <- provDebugR:::.get.valid.query.var(p.full, q7))
	c24 <- utils::capture.output(
		c23 <- provDebugR:::.get.valid.query.var(p.vars, q7))
	
	c26 <- utils::capture.output(                                      # script num is not an integer
		c25 <- provDebugR:::.get.valid.query.var(p.full, q8))
	c28 <- utils::capture.output(
		c27 <- provDebugR:::.get.valid.query.var(p.vars, q8))
	
	# TEST: returned values
	expect_null(c1)
	expect_null(c2)
	expect_null(c3)
	expect_null(c5)
	expect_null(c7)
	expect_null(c9)
	expect_null(c11)
	expect_null(c13)
	expect_null(c15)
	expect_null(c17)
	expect_null(c19)
	expect_null(c21)
	expect_null(c23)
	expect_null(c25)
	expect_null(c27)
	
	# TEST: output messages
	expect_true(nchar(paste(c4, collapse='\n')) > 0)
	expect_true(nchar(paste(c6, collapse='\n')) > 0)
	expect_true(nchar(paste(c8, collapse='\n')) > 0)
	expect_true(nchar(paste(c10, collapse='\n')) > 0)
	expect_true(nchar(paste(c12, collapse='\n')) > 0)
	expect_true(nchar(paste(c14, collapse='\n')) > 0)
	expect_true(nchar(paste(c16, collapse='\n')) > 0)
	expect_true(nchar(paste(c18, collapse='\n')) > 0)
	expect_true(nchar(paste(c20, collapse='\n')) > 0)
	expect_true(nchar(paste(c22, collapse='\n')) > 0)
	expect_true(nchar(paste(c24, collapse='\n')) > 0)
	expect_true(nchar(paste(c26, collapse='\n')) > 0)
	expect_true(nchar(paste(c28, collapse='\n')) > 0)
})

# .get.valid.query.var - some valid, some invalid queries
test_that(".get.valid.query.var (some valid, some invalid queries)",
{
	# POS.NODES 
	# cols: d.id, p.id, name, valType, startLine, scriptNum
	p.full <- system.file("testexpected", "posVar_typeChanges_full.csv", package = "provDebugR")
	p.vars <- system.file("testexpected", "posVar_typeChanges_vars.csv", package = "provDebugR")
	
	p.full <- read.csv(p.full, header = TRUE, row.names = 1, stringsAsFactors = FALSE)
	p.vars <- read.csv(p.vars, header = TRUE, row.names = 1, stringsAsFactors = FALSE)
	
	# QUERIES 
	# cols: name, valType, startLine, scriptNum
	q1 <- data.frame(name = c('a','s'),                 # start line queries
					 valType = c("all","all"),
					 startLine = c(40,40),
					 scriptNum = c(1,1),
					 stringsAsFactors = FALSE)
	q2 <- data.frame(name = c('h','s'),                 # container queries
					 valType = c('vector','vector'),
					 startLine = c(NA,NA),
					 scriptNum = c(1,1),
					 stringsAsFactors = FALSE)
	q3 <- data.frame(name = c('h','s'),                 # type queries
					 valType = c('integer','integer'),
					 startLine = c(NA,NA),
					 scriptNum = c(1,1),
					 stringsAsFactors = FALSE)
	q4 <- data.frame(name = c('h','s'),                 # special valType queries
					 valType = c('factor','factor'),
					 startLine = c(NA,NA),
					 scriptNum = c(1,1),
					 stringsAsFactors = FALSE)
	q5 <- data.frame(name = c('a','invalid','dev.2'),   # name queries
					 valType = c("all","all","all"),
					 startLine = c(NA,NA,NA),
					 scriptNum = c(1,1,1),
					 stringsAsFactors = FALSE)
	
	# CASES
	c1 <- provDebugR:::.get.valid.query.var(p.full, q1, forward = FALSE)    # start line queries
	c2 <- provDebugR:::.get.valid.query.var(p.full, q1, forward = TRUE)
	c3 <- provDebugR:::.get.valid.query.var(p.vars, q1, forward = FALSE)
	c4 <- provDebugR:::.get.valid.query.var(p.vars, q1, forward = TRUE)
	
	c5 <- provDebugR:::.get.valid.query.var(p.full, q2, forward = FALSE)    # container queries
	c6 <- provDebugR:::.get.valid.query.var(p.full, q2, forward = TRUE)
	c7 <- provDebugR:::.get.valid.query.var(p.vars, q2, forward = FALSE)
	c8 <- provDebugR:::.get.valid.query.var(p.vars, q2, forward = TRUE)
	
	c9 <- provDebugR:::.get.valid.query.var(p.full, q3, forward = FALSE)    # type queries
	c10 <- provDebugR:::.get.valid.query.var(p.full, q3, forward = TRUE)
	c11 <- provDebugR:::.get.valid.query.var(p.vars, q3, forward = FALSE)
	c12 <- provDebugR:::.get.valid.query.var(p.vars, q3, forward = TRUE)
	
	c13 <- provDebugR:::.get.valid.query.var(p.full, q4, forward = FALSE)   # special valType queries
	c14 <- provDebugR:::.get.valid.query.var(p.full, q4, forward = TRUE)
	c15 <- provDebugR:::.get.valid.query.var(p.vars, q4, forward = FALSE)
	c16 <- provDebugR:::.get.valid.query.var(p.vars, q4, forward = TRUE)
	
	c17 <- provDebugR:::.get.valid.query.var(p.vars, q5, forward = FALSE)   # name queries
	c18 <- provDebugR:::.get.valid.query.var(p.vars, q5, forward = TRUE)
	c19 <- provDebugR:::.get.valid.query.var(p.full, q5, forward = FALSE)
	c20 <- provDebugR:::.get.valid.query.var(p.full, q5, forward = TRUE)
	
	# EXPECTED
	# cols: d.id, name, valType, startLine, scriptNum
	e1 <- cbind('d.id' = 'd23',             # start line queries
				q1[-1, ], 
				stringsAsFactors = FALSE)
	
	e2 <- cbind('d.id' = 'd20',             # container queries (h,s,vector)
				q2[-2, ], 
				stringsAsFactors = FALSE)
	e3 <- cbind('d.id' = 'd15',
				q2[-2, ], 
				stringsAsFactors = FALSE)
	
	e4 <- cbind('d.id' = 'd20',             # type queries (h,s,integer)
				q3[-2, ], 
				stringsAsFactors = FALSE)
	e5 <- cbind('d.id' = 'd19',
				q3[-2, ], 
				stringsAsFactors = FALSE)
	
	e6 <- cbind('d.id' = 'd24',             # special valType queries (h,s,factor)
				q4[-1, ], 
				stringsAsFactors = FALSE)
	
	e7 <- cbind('d.id' = 'd1',              # name queries (a,invalid,dev.2)
				q5[c(-2,-3), ], 
				stringsAsFactors = FALSE)
	e8 <- cbind('d.id' = c('d1','d4'),
				q5[-2, ], 
				stringsAsFactors = FALSE)
	e9 <- cbind('d.id' = c('d1','d2'),
				q5[-2, ], 
				stringsAsFactors = FALSE)
	
	# TEST
	expect_equivalent(c1,e1)    # start line queries
	expect_equivalent(c2,e1)
	expect_equivalent(c3,e1)
	expect_equivalent(c4,e1)
	
	expect_equivalent(c5,e2)    # container queries
	expect_equivalent(c6,e3)
	expect_equivalent(c7,e2)
	expect_equivalent(c8,e3)
	
	expect_equivalent(c9,e4)    # type queries
	expect_equivalent(c10,e5)
	expect_equivalent(c11,e4)
	expect_equivalent(c12,e5)
	
	expect_equivalent(c13,e6)   # special valType queries
	expect_equivalent(c14,e6)
	expect_equivalent(c15,e6)
	expect_equivalent(c16,e6)
	
	expect_equivalent(c17,e7)   # name queries
	expect_equivalent(c18,e7)
	expect_equivalent(c19,e8)
	expect_equivalent(c20,e9)
})
