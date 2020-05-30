library(testthat)
library(provDebugR)

context("debug.view")

# === THE TESTS ============================================================== #

# debug.view
test_that("debug.view",
{
	skip("tbd")
})

# .view.var
test_that("debug.view - .view.var",
{
	# CASES
	# var.env, data.dir
	var.env <- new.env()
	data.dir <- dirname(system.file("testdata", "loadDF_full.RObject", package = "provDebugR"))
	
	# Provenance directory not found
	n1 <- "var1"
	v1 <- "data/plot.pdf"
	t1 <- "{\"container\":\"vector\", \"dimension\":[1], \"type\":[\"character\"]}"
	
	c1 <- provDebugR:::.view.var(var.env, n1, v1, t1, "doesNotExist/data")
	
	# File not found (csv/txt/RObject)
	n2 <- "var2"
	v2 <- "data/doesNotExist.csv"
	t2 <- "{\"container\":\"vector\", \"dimension\":[1], \"type\":[\"character\"]}"
	
	c2 <- provDebugR:::.view.var(var.env, n2, v2, t2, data.dir)
	
	# File not found (other file types)
	n3 <- "var3"
	v3 <- "data/doesNotExist.pdf"
	t3 <- "{\"container\":\"vector\", \"dimension\":[1], \"type\":[\"character\"]}"
	
	c3 <- provDebugR:::.view.var(var.env, n3, v3, t3, data.dir)
	
	# Not a snapshot
	n4 <- "var4"
	v4 <- "Not a snapshot!"
	t4 <- "{\"container\":\"vector\", \"dimension\":[1], \"type\":[\"character\"]}"
	
	c4 <- provDebugR:::.view.var(var.env, n4, v4, t4, data.dir)
	
	# csv, txt, RObject
	n5 <- "var5"
	v5 <- "data/loadMatrix_full.csv"
	t5 <- "{\"container\":\"matrix\", \"dimension\":[10,10], \"type\":[\"integer\"]}"
	
	c5 <- provDebugR:::.view.var(var.env, n5, v5, t5, data.dir)
	
	# Partial
	n6 <- "var6"
	v6 <- "data/loadMatrix_PARTIAL.csv"
	t6 <- "{\"container\":\"matrix\", \"dimension\":[10,100], \"type\":[\"integer\"]}"
	
	c6 <- provDebugR:::.view.var(var.env, n6, v6, t6, data.dir)
	
	# Other file types
	n7 <- "var7"
	v7 <- "data/plot.pdf"
	t7 <- "{\"container\":\"vector\", \"dimension\":[1], \"type\":[\"character\"]}"
	
	c7 <- provDebugR:::.view.var(var.env, n7, v7, t7, data.dir)
	
	
	# EXPECTED
	s1 <- "Provenance directory not found."
	s2 <- "File not found."
	s3 <- "File not found."
	s4 <- NA
	s5 <- NA
	s6 <- "PARTIAL"
	s7 <- NA
	
	e1 <- v1
	e2 <- v2
	e3 <- v3
	e4 <- v4
	e5 <- matrix(c(1:100), 10)
	e6 <- matrix(c(1:1000), 10)[1,]
	
	
	# TEST
	expect_equal(c1, s1)
	expect_equal(c2, s2)
	expect_equal(c3, s3)
	expect_equal(c4, s4)
	expect_equal(c5, s5)
	expect_equal(c6, s6)
	expect_equal(c7, s7)
	
	expect_equivalent(ls(var.env), paste0("var", c(1:6)))
	
	expect_equivalent(var.env$var1, e1)
	expect_equivalent(var.env$var2, e2)
	expect_equivalent(var.env$var3, e3)
	expect_equivalent(var.env$var4, e4)
	expect_equivalent(var.env$var5, e5)
	expect_equivalent(var.env$var6, e6)
})

# .load.RObject
test_that("debug.view - .load.RObject",
{
	# CASES (data frame, vector, matrix, array)
	# var.env
	var.env <- new.env()
	
	# data frame
	f1 <- system.file("testdata", "loadDF_full.RObject", package = "provDebugR")
	n1 <- "df"
	
	provDebugR:::.load.RObject(f1, var.env, n1)
	
	# vector
	f2 <- system.file("testdata", "loadVector_full.RObject", package = "provDebugR")
	n2 <- "v"
	
	provDebugR:::.load.RObject(f2, var.env, n2)
	
	# matrix
	f3 <- system.file("testdata", "loadMatrix_full.RObject", package = "provDebugR")
	n3 <- "m"
	
	provDebugR:::.load.RObject(f3, var.env, n3)
	
	# array
	f4 <- system.file("testdata", "loadArray_full.RObject", package = "provDebugR")
	n4 <- "a"
	
	provDebugR:::.load.RObject(f4, var.env, n4)
	
	
	# EXPECTED
	e1 <- data.frame(a = c(1:5), b = as.character(c(11:15)), stringsAsFactors = FALSE)
	e2 <- c(1:100)
	e3 <- matrix(c(1:100), 10)
	e4 <- array(c(1:100), dim = c(20,50,30))
	
	
	# TEST
	expect_equivalent(ls(var.env), c("a","df","m","v"))
	
	expect_equivalent(var.env$df, e1)
	expect_equivalent(var.env$v, e2)
	expect_equivalent(var.env$m, e3)
	expect_equivalent(var.env$a, e4)
})

# .load.csv
test_that("debug.view - .load.csv",
{
	# CASES (data frame, vector, matrix, unidentifiable)
	# var.env
	var.env <- new.env()
	
	# data frame (full.path, var.name, var.value, val.type)
	f1 <- system.file("testdata", "loadDF_full.csv", package = "provDebugR")
	n1 <- "df1"
	v1 <- "data frame 1"
	t1 <- "{\"container\":\"data_frame\", \"dimension\":[5,2], \"type\":[\"integer\",\"character\"]}"
	
	f2 <- system.file("testdata", "loadDF_PARTIAL.csv", package = "provDebugR")
	n2 <- "df2"
	v2 <- "data frame 2"
	t2 <-"{\"container\":\"data_frame\", \"dimension\":[50,2], \"type\":[\"integer\",\"character\"]}"
	
	provDebugR:::.load.csv(f1, var.env, n1, v1, t1)
	provDebugR:::.load.csv(f2, var.env, n2, v2, t2)
	
	# vector (full.path, var.name, var.value, val.type)
	f3 <- system.file("testdata", "loadVector_full.csv", package = "provDebugR")
	n3 <- "v1"
	v3 <- "vector 1"
	t3 <- "{\"container\":\"vector\", \"dimension\":[100], \"type\":[\"integer\"]}"
	
	f4 <- system.file("testdata", "loadVector_PARTIAL.csv", package = "provDebugR")
	n4 <- "v2"
	v4 <- "vector 2"
	t4 <- "{\"container\":\"vector\", \"dimension\":[1000], \"type\":[\"integer\"]}"
	
	provDebugR:::.load.csv(f3, var.env, n3, v3, t3)
	provDebugR:::.load.csv(f4, var.env, n4, v4, t4)
	
	# matrix (full.path, var.name, var.value, val.type)
	f5 <- system.file("testdata", "loadMatrix_full.csv", package = "provDebugR")
	n5 <- "m1"
	v5 <- "matrix 1"
	t5 <- "{\"container\":\"matrix\", \"dimension\":[10,10], \"type\":[\"integer\"]}"
	
	f6 <- system.file("testdata", "loadMatrix_PARTIAL.csv", package = "provDebugR")
	n6 <- "m2"
	v6 <- "matrix 2"
	t6 <- "{\"container\":\"matrix\", \"dimension\":[10,100], \"type\":[\"integer\"]}"
	
	provDebugR:::.load.csv(f5, var.env, n5, v5, t5)
	provDebugR:::.load.csv(f6, var.env, n6, v6, t6)[ ,1]
	
	# unidentifiable (full.path, var.name, var.value, val.type)
	f7 <- f5
	n7 <- "u1"
	v7 <- "unidentifiable 1"
	t7 <- "{\"container\":\"unidentifiable\", \"dimension\":[10,10], \"type\":[\"integer\"]}"
	
	f8 <- f6
	n8 <- "u2"
	v8 <- "unidentifiable 2"
	t8 <- "{\"container\":\"unidentifiable\", \"dimension\":[10,100], \"type\":[\"integer\"]}"
	
	provDebugR:::.load.csv(f7, var.env, n7, v7, t7)
	provDebugR:::.load.csv(f8, var.env, n8, v8, t8)
	
	
	# EXPECTED (data frame, vector, matrix, array, unidentifiable)	
	e1 <- data.frame(a = c(1:5), b = as.character(c(11:15)), stringsAsFactors = FALSE)
	e2 <- e1
	
	e3 <- c(1:100)
	e4 <- c(1:19)
	
	e5 <- matrix(c(1:100), 10)
	e6 <- matrix(c(1:1000), 10)[1,]
	
	e7 <- "unidentifiable 1"
	e8 <- "unidentifiable 2"
	
	
	# TEST (data frame, vector, matrix, unidentifiable)
	expect_equivalent(ls(var.env), c("df1","df2","m1","m2","u1","u2","v1","v2"))
	
	expect_equivalent(var.env$df1, e1)
	expect_equivalent(var.env$df2, e2)
	expect_equivalent(var.env$v1, e3)
	expect_equivalent(var.env$v2, e4)
	expect_equivalent(var.env$m1, e5)
	expect_equivalent(var.env$m2, e6)
	expect_equivalent(var.env$u1, e7)
	expect_equivalent(var.env$u2, e8)
		
	# ARRAYS NOTE
	skip("arrays are very hard to test as they are written and read very differently, unfortunately.")
})

# .load.txt
test_that("debug.view - .load.txt",
{
	# Case
	full.path <- system.file("testdata", "loadTxt.txt", package = "provDebugR")
	var.env <- new.env()
	var.name <- "txtFile"
	
	provDebugR:::.load.txt(full.path, var.env, var.name)
	
	# Expected
	e1 <- "This is\na text document\nwithout an extra line."
	
	# Test
	vars <- ls(var.env)
	expect_equal(length(vars), 1)
	expect_equal(vars[1], var.name)
	expect_equal(var.env$txtFile, e1)
})
