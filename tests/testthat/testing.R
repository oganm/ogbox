context('general error check')

test_that('purge unable to delete objects', {
    a = 1
    purge()
    expect_error(a,"object 'a' not found")
})


