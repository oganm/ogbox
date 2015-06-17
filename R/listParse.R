listParse <-
function (daList,daArray){
    out = ''
    for (i in daArray){
        out = paste0(out , '[[' ,  daArray[i] , ']]')
    }
    eval(parse(text=paste0('daList' , out)))
}
