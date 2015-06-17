findInList <-
function(object, aList){
    indices = vector()
    for (i in 1:length(aList)){
        if (object %in% aList[[i]]){
            indices = c(indices, i)
        }
    }
    return(indices)
}
