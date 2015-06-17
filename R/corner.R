corner <-
function(x){
    row = 10
    col = 10
    if (ncol(x)<10){
        col = ncol(x)
    }
    if (nrow(x)<10){
        row = nrow(x)
    }
    x[1:row,1:col]
}
