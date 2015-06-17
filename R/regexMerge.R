regexMerge <-
function(regexList){
    paste0('(',paste0(gsmFind('GSE65135', 'lymph'),collapse=')|('), ')')
}
