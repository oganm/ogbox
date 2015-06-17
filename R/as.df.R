as.df <-
function (x, row.names = NULL, optional = FALSE, ...) 
{
    if (is.null(x)) 
        return(as.data.frame(list()))
    UseMethod("as.data.frame")
}
