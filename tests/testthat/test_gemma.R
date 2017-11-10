# context('gemma error check')
# 
# 
# test_that('getGemmaAnnot', {
#     temp = tempfile()
#     if(file.exists(temp)){
#         file.remove(temp)
#     }
#     expect_that(getGemmaAnnot('GPL8',temp),testthat::equals(TRUE))
#     
#     expect_that(getGemmaAnnot('GPL8',temp),testthat::equals(FALSE))
#     
#     expect_that(getGemmaAnnot('GPL8',temp,overwrite = TRUE),testthat::equals(TRUE))
#     
# })
# 
# test_that('annotations',{
#     temp = tempfile()
#     if(file.exists(temp)){
#         file.remove(temp)
#     }
#     getGemmaAnnot('GPL8',temp)
#     
#     genes = c('Napb','Cdkn1a')
#     probes =  c(884,885,328,83)
#     testthat::expect_equal(gemmaProbesetMatch(genes,temp),
#                            data.frame(Gene.Symbol  =c("Napb" ,  "Napb",   "Cdkn1a" ,"Cdkn1a"),
#                                       Probe = probes,stringsAsFactors = FALSE))
#     
#     testthat::expect_equal(gemmaProbesetMatch(genes %>% rev,temp),
#                            data.frame(Gene.Symbol  =c("Napb" ,  "Napb",   "Cdkn1a" ,"Cdkn1a") %>% rev,
#                                       Probe = c(probes[3:4],probes[1:2]),stringsAsFactors = FALSE))
#     
#     testthat::expect_equal(gemmaGeneMatch(probes,temp),c("Napb" ,  "Napb",   "Cdkn1a" ,"Cdkn1a"))
#     
# })
# 
