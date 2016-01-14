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
    expect_that(intersectMult(list = list(a=c(1,3,4),b=c(3,4,5), c = c(3,4,1))),
                equals(c(3,4)))
    expect_that(intersectMult(c(1,3,4), c(3,4,5), c = c(3,4,1)),
                equals(c(3,4)))
})

test_that('rn2col-col2rn',{
    df = data.frame(a=c('a','b','c'),b=c(1,2,3))
    expect_that(rownames(df2<-col2rn(df)) , equals(c('a','b','c')))
    expect_that(rn2col(df2) ,
                equals(data.frame(rownames=c('a','b','c'),b=c(1,2,3),
                                  stringsAsFactors=F,
                                  row.names= c('a','b','c'))))
})

test_that('sepExpr fails to separate data frames',{
    df = data.frame(a = c('asd','gfa','sad'), b = c('qwe', 'ewq', 'rew') , c = c(1.1,2.2,3.3), d = c(2.2,3.3,4.4))
    seperated = sepExpr(df)
    expect_that(seperated[[1]],
                equals(data.frame(a = c('asd','gfa','sad'), b = c('qwe', 'ewq', 'rew'))))
    expect_that(seperated[[2]],
                equals(data.frame(c = c(1.1,2.2,3.3), d = c(2.2,3.3,4.4))))
})

test_that('purge fails do delete',{
    a = 23
    purge()
    expect_error(a,'not found')
})


test_that('gsubMult',{
    str = 'asdfg'
    expect_that(gsubMult(c('a','s'),c('m','n'),str),
                equals('mndfg'))
})

# test_that('geom_ogboxvio', {
#     expect_that(class(geom_ogboxvio()),equals('list'))
#     expect_that(length(geom_ogboxvio()), equals(4))
# })
