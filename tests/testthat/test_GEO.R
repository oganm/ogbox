context('GEO error check')


test_that('gsmDown', {
    temp = tempfile()
    temp2 = tempfile()
    expect_that(a<-gsmDown('GSM1026376',temp),gives_warning('has mutliple files'))
    expect_that(a,testthat::equals(F))
    expect_that(a<-gsmDown('GSM1539691',temp),gives_warning("doesn't have a file attached"))
    expect_that(a,testthat::equals(F))
    expect_that(gsmDown('GSM86787',temp2),testthat::equals(T))
})

test_that('gsmFind >500',{
   expect_equal(gsmFind('GSE14468',regex = '6455'),'GSM361359')
})

test_that('gsmFind with regex merge',{
    expect_that(gsmFind('GSE29949','Brain mic'), testthat::equals(c( "GSM741192", "GSM741193", "GSM741194")))
    expect_equal(gsmFind('GSE69340',regex = regexMerge(c('OLIG2','NEK7'))),
                 c("GSM1698231", "GSM1698232", "GSM1698233", "GSM1698237", "GSM1698238" ,"GSM1698239"))
    expect_equal(gsmFind('GSE42021', regex = regexMerge(c('04_(+) 24hi', '22_(-) 24hi'), exact = T)),
                 c("GSM1030712", "GSM1030715"))
})

test_that('whichGSE', {
    expect_equal(whichGSE('GSM874480'), c("GSE35758", "GSE35766"))
    expect_equal(whichGSE('GSM63015'), 'GSE2882')
})

test_that('softDown and softParser',{
    tmp = tempfile()
    softDown('GSE28642',tmp)
    
    expect_equal(softDown('GSE28642',tmp), FALSE)
    
    softDataWithExp = softParser(tmp, expression = TRUE)
    softData = softParser(tmp)
    expect_that(softData, is_a('data.frame'))
    expect_equal(nrow(softData),4)
    expect_that(softDataWithExp, is_a('list'))
})


