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

# .get.output.var (?)

# === TESTING SHARED FUNCTIONS =============================================== #
# --- functions shared with debug.lineage ---

# .get.pos.var - general case
test_that(".get.pos.var (valid)",
{
	# CASE: data nodes in different scripts
	# test for data nodes and variables
	json <- system.file("testdata", "exceptions.json", package = "provDebugR")
	
	provDebugR:::.clear()
	prov.debug.file(json)
	
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
	
	
	# TODO - case: fromEnv
})

# TODO - .get.pos.var
test_that(".get.pos.var (invalid)", 
{
	# no data nodes
	
	# no variables
})

# .get.query.var - valid queries
test_that(".get.query.var (valid queries)",
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
	c3 <- provDebugR:::.get.query.var(c("x","y"), start.line = 5)
	
	e3 <- data.frame(name = c("x","y"), 
					 valType = c(NA,NA),
					 startLine = c(5,5), 
					 scriptNum = c(1,1), 
					 stringsAsFactors = FALSE)
	expect_equivalent(c3, e3)
	
	# same number of objects and start line numbers
	c4 <- provDebugR:::.get.query.var(c("x","z","y"), start.line = c(3:5))
	
	e4 <- data.frame(name = c("x","z","y"), 
					 valType = c(NA,NA,NA),
					 startLine = c(3:5), 
					 scriptNum = c(1,1,1), 
					 stringsAsFactors = FALSE)
	expect_equivalent(c4, e4)
	
	# same number of objects and start line numbers, with NA as a line number
	c5 <- provDebugR:::.get.query.var(c("x","y","z"), start.line = c(3,NA,5))
	
	e5 <- data.frame(name = c("x","y","z"), 
					 valType = c(NA,NA,NA),
					 startLine = c(3,NA,5), 
					 scriptNum = c(1,1,1), 
					 stringsAsFactors = FALSE)
	expect_equivalent(c5, e5)
	
	# val.type and script.num not their default values
	c6 <- provDebugR:::.get.query.var(c("x","y"), 
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

# .get.query var - invalid queries
test_that(".get.query.var (invalid queries)",
{
	# no queried variables
	c1 <- provDebugR:::.get.query.var(NULL)
	expect_null(c1)
	
	# queried more than 1 script numbers
	expect_warning(c2 <- provDebugR:::.get.query.var("x", script.num = c(1:2)))
	expect_null(c2)
	
	# queried more than 1 valTypes
	expect_warning(c3 <- provDebugR:::.get.query.var("x", val.type = c("logical", "character")))
	expect_null(c3)
	
	# objects and start line numbers queried are greater than 1, and do not match
	obj <- c("x","y")
	lines <- c(1:3)
	
	expect_warning(c4 <- provDebugR:::.get.query.var(obj, start.line = lines))
	expect_null(c4)
})

# .get.valid.var - all valid queries
test_that(".get.valid.var (all valid queries)",
{
	# POS.NODES
	# cols: d.id, p.id, name, valType, startLine, scriptNum
	p.full <- system.file("testexpected", "posVar_typeChanges_full.csv", package = "provDebugR")
	p.vars <- system.file("testexpected", "posVar_typeChanges_vars.csv", package = "provDebugR")
	
	p.full <- read.csv(p.full, header = TRUE, row.names = 1, stringsAsFactors = FALSE)
	p.vars <- read.csv(p.vars, header = TRUE, row.names = 1, stringsAsFactors = FALSE)
	
	
	# QUERIES
	# cols: name, valType, startLine, scriptNum
	
	
	# returned columns: d.id, p.id, name, valType, startLine, scriptNum
	
	# all can be done using typeChanges
	
	
})

# .get.valid.var - all invalid queries
test_that(".get.valid.var (all invalid queries)",
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
					 "valType"= NA, 
					 "startLine" = NA, 
					 "scriptNum" = 1,
					 stringsAsFactors = FALSE)
	q3 <- data.frame("name" = "dev.2",           # is data node, but not variable
					 "valType"= NA, 
					 "startLine" = NA, 
					 "scriptNum" = 1,
					 stringsAsFactors = FALSE)
	q4 <- data.frame("name" = c("a","s"),        # invalid valType
					 "valType"= c("language","language"), 
					 "startLine" = c(NA,NA), 
					 "scriptNum" = c(1,1),
					 stringsAsFactors = FALSE)
	q5 <- data.frame("name" = c("a","s"),        # invalid start lines
					 "valType"= c(NA,NA), 
					 "startLine" = c(50,2), 
					 "scriptNum" = c(1,1),
					 stringsAsFactors = FALSE)
	q6 <- data.frame("name" = "a",               # invalid script num
					 "valType"= NA, 
					 "startLine" = NA, 
					 "scriptNum" = 5,
					 stringsAsFactors = FALSE)
	
	# CASES
	c1 <- provDebugR:::.get.valid.var(p.full, q1)   # no query
	c2 <- provDebugR:::.get.valid.var(p.vars, q1)
	
	c3 <- provDebugR:::.get.valid.var(p.full, q2)   # data node with name does not exist
	c4 <- provDebugR:::.get.valid.var(p.vars, q2)
	
	c5 <- provDebugR:::.get.valid.var(p.vars, q3)   # is data node, but not variable
	
	c6 <- provDebugR:::.get.valid.var(p.full, q4)   # invalid valType
	c7 <- provDebugR:::.get.valid.var(p.vars, q4)
	
	c8 <- provDebugR:::.get.valid.var(p.full, q5)   # invalid start lines
	c9 <- provDebugR:::.get.valid.var(p.vars, q5)
	
	c11 <- utils::capture.output(c10 <- provDebugR:::.get.valid.var(p.full, q6))   # invalid script num
	c13 <- utils::capture.output(c12 <- provDebugR:::.get.valid.var(p.vars, q6))
	
	# TEST: returned values
	expect_null(c1)
	expect_null(c2)
	expect_null(c3)
	expect_null(c4)
	expect_null(c5)
	expect_null(c6)
	expect_null(c7)
	expect_null(c8)
	expect_null(c9)
	expect_null(c10)
	expect_null(c12)
	
	# TEST: output messages
	c11 <- paste(c11, collapse = '\n')
	c13 <- paste(c13, collapse = '\n')
	
	expect_true(nchar(c11) > 0)
	expect_true(nchar(c13) > 0)
})

# .get.valid.var - some valid, some invalid queries
test_that(".get.valid.var (some valid, some invalid queries)",
{
	# pos.nodes columns: d.id, p.id, name, valType, startLine, scriptNum
	# query columns: name, valType, startLine, scriptNum
	# returned columns: d.id, p.id, name, valType, startLine, scriptNum
	
	# all can be done using typeChanges
	
	
})
