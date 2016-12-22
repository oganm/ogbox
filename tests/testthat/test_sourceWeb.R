test_that('sourceGithub',{
    sourceGithub('oganm/toSource/homologene.R')
    expect_that(typeof(mouse2human),testthat::equals('closure'))
})

test_that('loadGithub',{
    loadGithub('oganm/neuroExpressoAnalysis/data/mouseMarkerGenes.rda')
    expect_that(typeof(mouseMarkerGenes), testthat::equals('list'))
})