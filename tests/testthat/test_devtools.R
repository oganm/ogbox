context('devtools')

test_that('getVersion',{
    testthat::expect_equal(getVersion(),'1.0')
})

test_that('setVersion',{
    setVersion('1.2')
    testthat::expect_equal(getVersion(),'1.2')
    setVersion('1.0')
})