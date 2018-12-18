#' Get last value
#' @description A shorter way to get the \code{.Last.value}
#' @export
ans = function(){
    base::.Last.value
}


# function acronyms ----
#' @export
len = length
#' @export
as.char = as.character
#' @export
as.df = as.data.frame
#' @export
as.num = as.numeric
#' @export
rn = rownames
#' @export
cn = colnames

#' @export
`rn<-` = `rownames<-`
#' @export
`cn<-` = `colnames<-`

#' @export
install.pakcages = function(...){
    # don't insult strangers
    if(Sys.info()['nodename'] %in%  c('nelson.msl.ubc.ca','pavdesk-26','oganm','oganGR')){
        warning('you spelled it wrong moron')
    }
    install.packages(...)
}

#' @export
insPack = install.packages