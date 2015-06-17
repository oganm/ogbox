mergeList <-
function(aList,bList,forceUnique=T){
    allNames = unique(c(names(aList),names(bList)))
    outList = vector(mode= "list",length = length(allNames))
    names(outList) = allNames
    outList = sapply(allNames,function(x){
        out=(c(aList[[x]],bList[[x]]))
        if (forceUnique){
            out = unique(out)
        }
        return(out)
    })
    return(outList)
}
