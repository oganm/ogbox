trimElement <-
function (aVector,e){
    return(aVector[!(aVector %in% e)])
}
