library(testthat)
library(provDebugR)

context("Lineage")

json <- system.file("testdata", "test.json", package = "provDebugR")
prov.debug.file(json)

data.nodes <- provDebugR:::.debug.env$data.nodes

# .get.valid.args
test_that(".get.valid.args", 
{
	.get.valid.args <- provDebugR:::.get.valid.args
	pos.args <- as.list(unique(data.nodes$name))
	
	# all invalid arguments
	expect_null(.get.valid.args(pos.args, "invalid"))
	expect_null(.get.valid.args(pos.args, "invalid.1", c("invalid.2", "invalid.3")))
	
	# no arguments specified
	expect_equivalent(.get.valid.args(pos.args), pos.args)
	
	# all valid arguments
	expect_equivalent(.get.valid.args(pos.args, "x"), "x")
	expect_equivalent(.get.valid.args(pos.args, c("a", "x")), c("a", "x"))
	expect_equivalent(.get.valid.args(pos.args, "f", c("a", "x")), c("f", "a", "x"))
	expect_equivalent(.get.valid.args(pos.args, "x", c("a", "x")), c("a", "x"))  # remove duplicates 
	
	# some valid, some invalid arguments
	expect_equivalent(.get.valid.args(pos.args, "x", "invalid"), "x")
	expect_equivalent(.get.valid.args(pos.args, "x", c("invalid", "g")), c("x", "g"))
})

# .get.lineage
test_that(".get.lineage"
{
	.get.lineage <- provDebugR:::.get.lineage
	
	# backwards lineage
	expect_equivalent(.get.lineage("a", forward = FALSE), "p17")
	expect_equivalent(.get.lineage("x", forward = FALSE), c("p15", "p3"))
	
	# forwards lineage
	expect_equivalent(.get.lineage("val", forward = TRUE), "p14")
	expect_equivalent(.get.lineage("x", forward = TRUE), c("p4", "p6", "p14", "p7"))
})