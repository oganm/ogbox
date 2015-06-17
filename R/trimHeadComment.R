trimHeadComment <-
function(fileName, commentChar = '#',outFile = NULL){
    lines = readLines(fileName)
    i=1
    while (T){
        if (substr(lines[i],1,1)!=commentChar){
            break}
        i = i+1
    }
    if (is.null(outFile)){
        return(textConnection(paste(lines[i:length(lines)],collapse='\n')))
    } else {
        fileConn = file(outFile)
        writeLines(text = lines[i:length(lines)] , con = fileConn)
        close(fileConn)
        invisible(textConnection(paste(lines[i:length(lines)],collapse='\n')))
    }
}
