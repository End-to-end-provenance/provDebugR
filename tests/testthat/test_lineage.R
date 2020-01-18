library(testthat)

context("debug.lineage")

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

# all

# backwards lineage - name queries (valid and invalid)
# forwards lineage - name queries (valid and invalid)

# backwards lineage - start line queries (valid and invalid)
# forwards lineage - start line queries (valid and invalid)

# no lineage cases (var and not a var)



# helper function tests
# get lineage tests (esp forward tests)
# output test - for removed last row for no dev.off case