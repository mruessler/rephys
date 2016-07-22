context("C++ functions")

test_that("prunespikes prunes", {
	spikevector <- rep(1, 10)
	spikematrix <- cbind(spikevector, spikevector)
	expect_equal(sum(prunespikes(spikematrix, minisi = 2)), 10)
	expect_equal(sum(prunespikes(spikematrix, minisi = 5)), 4)
	expect_equal(sum(prunespikes(spikematrix, minisi = 9)), 2)
})
