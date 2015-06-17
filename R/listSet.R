listSet <-
function(daList,daArray ,something){
    name = substitute(daList)
    name = as.character(name)
    out = ''
    for (i in daArray){
        out = paste0(out , '[[' ,  i , ']]')
    }

    eval(parse(text = paste0(name, out, '<<-something')))
}
