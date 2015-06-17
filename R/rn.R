rn <-
function (x, do.NULL = TRUE, prefix = "row") 
{
    dn <- dimnames(x)
    if (!is.null(dn[[1L]])) 
        dn[[1L]]
    else {
        nr <- NROW(x)
        if (do.NULL) 
            NULL
        else if (nr > 0L) 
            paste0(prefix, seq_len(nr))
        else character()
    }
}
