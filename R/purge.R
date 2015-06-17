purge <-
function() {
    rm(list = ls(.GlobalEnv, all.names = T), envir = .GlobalEnv)
}
