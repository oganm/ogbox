trimNAs <-
function(aVector) {
    return(aVector[!is.na(aVector)])
}
