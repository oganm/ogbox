`%c%` <-
function (x, y){
    start = which(is.na(x))[1]
    x[start:(start+length(y) - 1)]= y
    return(x)
}
