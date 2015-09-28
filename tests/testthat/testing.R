context('general error check')
test_that("trimNAs can't trim NAs", {
    a = c(1,2,NA)
    expect_that(ogbox::trimNAs(a),equals(c(1,2)))
})



test_that("toColor defaults", {
    a = c('a','b','c','a')
    col = toColor(a)
    check  = list(cols = c( "#FF0000FF", "#00FF00FF", "#0000FFFF","#FF0000FF"),
                  palette = c(a="#FF0000FF",b= "#00FF00FF", c="#0000FFFF"))
    expect_that(col$palette,
                equals(check$palette))
})

test_that('toColor custom', {
    a = c('a','b','c','a')
    palette = c('a','b','c')
    col = toColor(a, palette)
    check  = list(cols = c( "a", "b", "c","a"),
                  palette = c(a="a",b= "b", c="c"))
})

test_that('list seperator', {
    list[a,b] = list(a=c(1,2,3),b=c(2,3,4)) 
    expect_that(a,
                equals(c(1,2,3)))
    expect_that(b,
                equals(c(2,3,4)))
})

test_that('gsmDown', {
    expect_that(a<-gsmDown('GSM1026376','hede'),gives_warning('has mutliple files'))
    expect_that(a,equals(F))
    expect_that(a<-gsmDown('GSM1539691','hede'),gives_warning("doesn't have a file attached"))
    expect_that(a,equals(F))
})
