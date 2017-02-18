context('gemma error check')


test_that('getGemmaAnnot', {
    temp = tempfile()
    if(file.exists(temp)){
        file.remove(temp)
    }
    expect_that(getGemmaAnnot('GPL8',temp),testthat::equals(TRUE))
    
    expect_that(getGemmaAnnot('GPL8',temp),testthat::equals(FALSE))
    
    expect_that(getGemmaAnnot('GPL8',temp,overwrite = TRUE),testthat::equals(TRUE))
    
})