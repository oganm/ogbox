test_that('sourceGithub',{
    sourceGithub('oganm/toSource/homologene.R')
    expect_that(typeof(mouse2human),testthat::equals('closure'))
    
    sourceGithub("oganm/neuroExpressoAnalysis/analysis/00.DownloadPreprocess/01.MouseCellTypeData.R#L150-L163")
    testthat::expect_that(typeof(regionHierarchy), testthat::equals('list'))
})

test_that('loadGithub',{
    loadGithub('oganm/neuroExpressoAnalysis/data/mouseMarkerGenes.rda')
    expect_that(typeof(mouseMarkerGenes), testthat::equals('list'))
})