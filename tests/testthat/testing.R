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

test_that('gsmFind',{
    expect_that(gsmFind('GSE29949','Brain mic'), equals(c( "GSM741192", "GSM741193", "GSM741194")))
})

test_that('sourceGithub',{
    sourceGithub(oganm,toSource,homologene)
    expect_that(typeof(mouse2human),equals('closure'))
})


test_that('intersectMult',{
    expect_that(intersectMult(list = list(a=c(1,3,4),b=c(3,4,5))),
                equals(c(3,4)))
    expect_that(intersectMult(c(1,3,4), c(3,4,5)),
                equals(c(3,4)))
})
