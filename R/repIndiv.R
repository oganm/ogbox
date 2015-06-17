repIndiv <-
function (aVector, n){
    output = vector(length = length(aVector) * n)
    step = 1
    for (i in aVector){
        output[(step * n - n + 1):(n * step)] = rep(i, n)
        step = step + 1
    }
    return(output)
}
