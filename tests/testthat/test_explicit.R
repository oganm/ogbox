context('explicit')

test_that('explicit',{
    temp = tempfile()
    cat("test_that('lol')\n",file = temp)
    explicit(temp)
    expect_that(readLines(temp),testthat::equals("testthat::test_that('lol')"))
})