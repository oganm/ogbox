toColor <-
function(daList, palette = NULL){
    daList = as.factor(daList)
    uniq = unique(daList)
    if (is.null(palette[1])){
        palette = rainbow(length(uniq))
    }
    if (is.null(names(palette))){
        names(palette) = uniq
    }
    cols = vector (length = length(daList))
    #to match palette names to uniq names so that custom naming is possible
    if (!is.null(names(palette))){
        palette = trimNAs(palette[match(uniq,names(palette))])
        names(palette) = uniq
    }
    
    for (i in 1:length(uniq)){
        cols[daList == uniq[i]]= palette[i]
    }
    out = list()
    out$cols = cols
    out$palette = palette
    
    return(out)
}
