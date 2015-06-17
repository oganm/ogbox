checkLines <-
function(daFile,lines,fun = readLines, ...){
    outAll = vector(mode= 'list',length = length(lines))
    for (i in 1:length(lines)){
        con = pipe(paste0("sed -n -e'",lines[i],"p' ",daFile))
        out = fun(con, ...)
        outAll[[i]] = out
    }
    return(outAll)
}
