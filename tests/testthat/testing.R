context('general error check')
agzinaSictigimin = 1
test_that("trimNAs can't trim NAs", {
    a = c(1,2,NA)
    expect_that(ogbox::trimNAs(a),equals(c(1,2)))
})



