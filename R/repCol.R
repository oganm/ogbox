repCol <-
function(x,n){
    matrix(rep(x,each=n), ncol=n, byrow=TRUE)
}
