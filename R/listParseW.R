listParseW <-
function (daList,daArray){
    out = ''
    if (length(daArray) > 1){
        for (i in daArray[1 : (length(daArray) - 1)]){
            out = paste0(out , '[[' ,  i , ']]')
        }
    }
    out = paste0(out ,'[', daArray[length(daArray)], ']')
    eval(parse(text=paste0('daList' , out)))
}
