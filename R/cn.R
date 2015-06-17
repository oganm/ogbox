cn <-
function (x, do.NULL = TRUE, prefix = "col") 
{
    if (is.data.frame(x) && do.NULL) 
        return(names(x))
    dn <- dimnames(x)
    if (!is.null(dn[[2L]])) 
        dn[[2L]]
    else {
        nc <- NCOL(x)
        if (do.NULL) 
            NULL
        else if (nc > 0L) 
            paste0(prefix, seq_len(nc))
        else character()
    }
}
