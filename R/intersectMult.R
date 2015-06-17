intersectMult <-
function (...){
    targets = list(...)
    
    out = intersect(targets[[1]],targets[[2]])
    if (length(targets)>=3){
        for (i in 3:(length(targets))){
            out = intersect(out, targets[[i]])
        }
    }
    return(out)
}
