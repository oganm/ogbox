#' Cantor Pairing
#' 
#' Performs cantor pairing on two integers
#' 
#' @param x First integer
#' @param y Second integer
#' @return Integer corresponding to the paired value
#' @export
cantor_pairing = function(x,y){
    x = bit64::as.integer64(x)
    y = bit64::as.integer64(y)
    z = (x + y) * (x + y + 1) / 2 + y
    
    tryCatch(8*z,warning = function(e){
        warning('This pair cannot be unpaired')
    })
    return(z)
    
}

#' @export
cantor_unpairing = function(z){
    w = as.integer64((-1 + sqrt(1+8*z))/2)
    t = (w^2+w)/2
    y = z-t
    x = w-y
    return(c(x,y))
}

#' @export
szudzik_pairing = function(x,y){
    x = bit64::as.integer64(x)
    y = bit64::as.integer64(y)
    if(x<y){
        z = y^2 + x
    } else{
       z =  x^2 + x + y
    }
    return(z)
}

#' @export
szudzik_unpairing = function(z){
    if((z - as.integer64(sqrt(z))^2) < sqrt(z)){
        x = z - as.integer64(sqrt(z))^2
        y = as.integer64(sqrt(z))
    } else {
        x = as.integer64(sqrt(z))
        y = z - as.integer64(sqrt(z))^2 - as.integer64(sqrt(z))
    }
    
    return(c(x,y))
}
