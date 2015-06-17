celFiles <-
function (..., listGzipped = FALSE) 
{
    files <- list.files(...)
    if (listGzipped) {
        return(files[grep("\\.[cC][eE][lL]\\.[gG][zZ]$|\\.[cC][eE][lL]$", 
            files)])
    }
    else {
        return(files[grep("\\.[cC][eE][lL]$", files)])
    }
}
