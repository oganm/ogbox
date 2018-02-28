#' Get version of a local package
#' @param pkg Pakcage to check version of. Defaults to local directory
#' @export
getVersion = function(pkg = '.'){
    lines = readLines('DESCRIPTION')
    lines[grepl('Version',lines)] %>% stringr::str_extract('[0-9].*$')
}

#' Set version of a local package
#' @param version Character for the new version
#' @param pkg Pakcage to set version of. Defaults to local directory
#' @export
setVersion = function(version, pkg = '.'){
    lines = readLines('DESCRIPTION')
    lines[grepl('Version',lines)] = paste('Version:',version)
    writeLines(lines,'DESCRIPTION')
}

#' Set version of a local package
#' @param Date Character for the new date
#' @param pkg Pakcage to set version of. Defaults to local directory
#' @export
setDate = function(date,pkg = '.'){
    lines = readLines('DESCRIPTION')
    lines[grepl('Date',lines)] = paste('Date:',date)
    writeLines(lines,'DESCRIPTION')
}