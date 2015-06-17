scale01 <-
function(x){
    x = (x - min(x))/(max(x)-min(x))
    return(x)
}
