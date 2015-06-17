insist <-
function(name){
    name = substitute(name)
    name = as.character(name)
    if (!require(name, character.only = T)) {
        install.packages(name)
        Sys.sleep(5)
        library(name, character.only = T, logical.return = F)
    }
}
