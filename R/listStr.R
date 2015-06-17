listStr <-
function(daArray){
    out = ''
    for (i in daArray[1 : length(daArray)]){

        out = paste0(out, '[[',  i, ']]')
    }
    return(out)
}
