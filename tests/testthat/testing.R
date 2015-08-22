context('general error check')
agzinaSictigimin = 1
test_that('purge unable to delete objects', {
    ogbox::purge()
    expect_error(print(agzinaSictigimin),"object 'agzinaSictigimin' not found")
})



