getParent <-
function(step = 1){
    wd = getwd()
    for (i in 1:step){
       wd = dirname(wd)
    }
    return(wd)
}
