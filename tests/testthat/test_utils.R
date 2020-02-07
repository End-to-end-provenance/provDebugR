library(testthat)
library(provDebugR)

context("Utility functions")

# .clear
# This is a function that is used only for testing purposes
test_that("Utility - .clear", 
{
	json <- system.file("testdata", "exceptions.json", package = "provDebugR")
	prov.debug.file(json)
	
	# ensure .debug.env has been changed
	expect_false(is.null(provDebugR:::.debug.env$prov))
	expect_false(is.null(provDebugR:::.debug.env$graph))
	expect_true(provDebugR:::.debug.env$has.graph)
	expect_false(is.null(provDebugR:::.debug.env$proc.nodes))
	expect_false(is.null(provDebugR:::.debug.env$data.nodes))
	expect_false(is.null(provDebugR:::.debug.env$data.proc))
	expect_false(is.null(provDebugR:::.debug.env$proc.data))
	
	# clear .debug.env
	provDebugR:::.clear()
	
	# ensure .debug.env has been returned to initial state
	expect_true(is.null(provDebugR:::.debug.env$prov))
	expect_true(is.null(provDebugR:::.debug.env$graph))
	expect_false(provDebugR:::.debug.env$has.graph)
	expect_true(is.null(provDebugR:::.debug.env$proc.nodes))
	expect_true(is.null(provDebugR:::.debug.env$data.nodes))
	expect_true(is.null(provDebugR:::.debug.env$data.proc))
	expect_true(is.null(provDebugR:::.debug.env$proc.data))
})

# .form.df
test_that("Utility - .form.df",
{
	# 1 row
	l1 <- list(data.frame(col1 = 1L, col2 = "str.1", stringsAsFactors = FALSE))
	c1 <- provDebugR:::.form.df(l1)
	
	e1 <- data.frame(col1 = 1L, col2 = "str.1", stringsAsFactors = FALSE)
	
	expect_equal(c1, e1)
	
	# 1 column
	l2 <- list(data.frame(col1 = 1L, stringsAsFactors = FALSE),
			   data.frame(col1 = 2L, stringsAsFactors = FALSE),
			   data.frame(col1 = c(3L,4L), stringsAsFactors = FALSE))
	c2 <- provDebugR:::.form.df(l2)
	
	e2 <- data.frame(col1 = as.integer(c(1:4)), stringsAsFactors = FALSE)
	
	expect_equal(c2, e2)
	
	# columns have different types
	l3 <- list(data.frame(col1 = 1L, col2 = "str.1", stringsAsFactors = FALSE),
			   data.frame(col1 = 2L, col2 = "str.2", stringsAsFactors = FALSE),
			   data.frame(col1 = as.integer(NA), col2 = "str.3", stringsAsFactors = FALSE))
	c3 <- provDebugR:::.form.df(l3)
	
	e3 <- data.frame(col1 = as.integer(c(1,2,NA)),
					 col2 = c("str.1", "str.2", "str.3"),
					 stringsAsFactors = FALSE)
	
	expect_equal(c3, e3)
	
	# columns have the same type
	l4 <- list(data.frame(col1 = 1L, col2 = 11L, stringsAsFactors = FALSE),
			   data.frame(col1 = 2L, col2 = 12L, stringsAsFactors = FALSE),
			   data.frame(col1 = 3L, col2 = 13L, stringsAsFactors = FALSE))
	c4 <- provDebugR:::.form.df(l4)
	
	e4 <- data.frame(col1 = as.integer(c(1:3)),
					 col2 = as.integer(c(11:13)),
					 stringsAsFactors = FALSE)
	
	expect_equal(c4, e4)
	
	# some elements have multiple rows
	l5 <- list(data.frame(col1 = 1L, col2 = "str.1", stringsAsFactors = FALSE),
			   data.frame(col1 = c(2L,3L,4L), col2 = c("str.2", "str.3", "str.4"), stringsAsFactors = FALSE),
			   data.frame(col1 = 5L, col2 = "str.5", stringsAsFactors = FALSE))
	c5 <- provDebugR:::.form.df(l5)
	
	e5 <- data.frame(col1 = as.integer(c(1:5)),
					 col2 = c("str.1", "str.2", "str.3", "str.4", "str.5"),
					 stringsAsFactors = FALSE)
	
	expect_equal(c5, e5)
})

# .to.int
test_that("Utility - .to.int",
{
	# already an integer
	# NA
	# booleans
	# decimals
	# integers as strings
	# non-integers as strings
	
	# cases
	c1 <- provDebugR:::.to.int(1L)           # already an integer
	c2 <- provDebugR:::.to.int(NA)           # NA
	c3 <- provDebugR:::.to.int("3")          # integer as a string
	
	c4 <- provDebugR:::.to.int(2.5)          # decimal
	c5 <- provDebugR:::.to.int(TRUE)         # booleans
	c6 <- provDebugR:::.to.int(FALSE)
	c7 <- provDebugR:::.to.int("a string")   # non-integers as strings
	c8 <- provDebugR:::.to.int("4.5")
	
	# expected
	e1 <- 1L
	e2 <- as.integer(NA)
	e3 <- 3L
	
	# test
	expect_equal(c1, e1)
	expect_equal(c2, e2)
	expect_equal(c3, e3)
	
	expect_null(c4)
	expect_null(c5)
	expect_null(c6)
	expect_null(c7)
	expect_null(c8)
})

# .find.num.loc
test_that("Utility - .find.num.loc",
{
	# matching numbers
	l1 <- as.integer(c(1:5))
	
	c1 <- provDebugR:::.find.num.loc(l1, 1L)
	c2 <- provDebugR:::.find.num.loc(l1, 3L)
	c3 <- provDebugR:::.find.num.loc(l1, 5L)
	
	expect_equal(c1, 1)
	expect_equal(c2, 3)
	expect_equal(c3, 5)
	
	# list has 1 number
	c4 <- provDebugR:::.find.num.loc(1L, 2L)
	expect_equal(c4, 1)
	
	# number can not be found
	l2 <- as.integer(c(11,13,15,17,19))
	
	c5 <- provDebugR:::.find.num.loc(l2, 12L)
	c6 <- provDebugR:::.find.num.loc(l2, 18L)
	
	expect_equal(c5, 1)
	expect_equal(c6, 4)
	
	# repeated numbers in list
	l3 <- as.integer(c(21,21,23,25,25,27,27,27,29,29))
	
	c7 <- provDebugR:::.find.num.loc(l3, 22L)
	c8 <- provDebugR:::.find.num.loc(l3, 26L)
	c9 <- provDebugR:::.find.num.loc(l3, 27L)
	c10 <- provDebugR:::.find.num.loc(l3, 28L)
	
	expect_equal(c7, 2)
	expect_equal(c8, 5)
	expect_equal(c9, 8)
	expect_equal(c10, 8)
	
	# out of bounds
	l4 <- as.integer(c(41,43,45))
	
	c11 <- provDebugR:::.find.num.loc(l4, 35L)
	c12 <- provDebugR:::.find.num.loc(l4, 50L)
	
	expect_equal(c11, 0)
	expect_equal(c12, 3)
})

# .get.p.id
test_that("Utility - .get.p.id",
{
	json <- system.file("testdata", "fromEnv.json", package = "provDebugR")
	
	provDebugR:::.clear()
	expect_warning(prov.debug.file(json))   # warning due to deleted prov folder
	
	# node edges
	proc.data <- provDebugR:::.debug.env$proc.data
	data.proc <- provDebugR:::.debug.env$data.proc
	
	# Cases
	c1 <- "d4"    # 1 node found (from output edge only)
	c2 <- "d14"   # 1 node found (from input edge only)
	c3 <- "d1"    # 1 node found (has both output and input edges)
	c4 <- "d2"    # multiple nodes found (multiple input edges)
	
	c1 <- provDebugR:::.get.p.id(c1, proc.data, data.proc)
	c2 <- provDebugR:::.get.p.id(c2, proc.data, data.proc)
	c3 <- provDebugR:::.get.p.id(c3, proc.data, data.proc)
	c4 <- provDebugR:::.get.p.id(c4, proc.data, data.proc)
	
	# Expected
	e1 <- "p5"
	e2 <- "p15"
	e3 <- "p3"
	e4 <- c("p4", "p6")
	
	# Test
	expect_equivalent(c1, e1)
	expect_equivalent(c2, e2)
	expect_equivalent(c3, e3)
	expect_equivalent(c4, e4)
})

# .remove.na.rows
test_that("Utility - .remove.na.rows",
{
	# cases
	df1 <- data.frame(col1 = integer(),                   # no rows
					  col2 = character(),
					  stringsAsFactors = FALSE)
	
	df2 <- data.frame(col1 = c(1:3),
					  col2 = c("item.1", NA, "item.3"),   # no NA rows
					  stringsAsFactors = FALSE)
	
	df3 <- data.frame(col1 = c(1,NA,3,4,NA),              # some NA rows
					  col2 = c(1,NA,3,4,NA),
					  stringsAsFactors = FALSE)
	
	df4 <- data.frame(col1 = as.integer(c(NA,NA)),        # all NA rows
					  col2 = as.character(c(NA,NA)),
					  stringsAsFactors = FALSE)
	
	c1 <- provDebugR:::.remove.na.rows(df1)
	c2 <- provDebugR:::.remove.na.rows(df2)
	c3 <- provDebugR:::.remove.na.rows(df3)
	c4 <- provDebugR:::.remove.na.rows(df4)
	
	# expected
	e1 <- df1
	e2 <- df2
	e3 <- df3[c(1,3,4), ]
	e4 <- df1
	
	# test
	expect_equivalent(c1, e1)
	expect_equivalent(c2, e2)
	expect_equivalent(c3, e3)
	expect_equivalent(c4, e4)
})

# .remove.null
test_that("Utility - .remove.null",
{
	# cases
	l1 <- as.list(c(1:5))              # no nulls
	l2 <- list(NULL, 1, 2, NULL, 3)    # with nulls
	l3 <- list(NULL, NULL, NULL)       # all nulls
	
	c1 <- provDebugR:::.remove.null(l1)
	c2 <- provDebugR:::.remove.null(l2)
	c3 <- provDebugR:::.remove.null(l3)
	
	# expected
	e1 <- l1
	e2 <- as.list(c(1:3))
	
	# test
	expect_equivalent(c1, e1)
	expect_equivalent(c2, e2)
	expect_true(length(c3) == 0)
})
