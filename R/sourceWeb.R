#' Source R files from github
#' @description sources R files from github
#' @param githubPath character. username/repository/pathToFile. Lines to excecute can be provided with github's #L1-L5 syntax. The function doesn't work with files that have ? # [ or ] in their addresses.
#' @param line numbers to excecute. Providing lines in both here and with githubPath will result in an error.
#' @param branch which branch to source from
#' @seealso \code{\link{loadURL}}, \code{\link{sourceGithub}}
#' @export
sourceGithub = function(githubPath,lines = NULL, branch = 'master'){
    path = strsplit(githubPath,'/')[[1]]
    user = path[1]
    repo = path[2]
    script = paste(path[3:length(path)], collapse = '/')
    pathLines = script %>% {strsplit(.,"#")[[1]][2]}
    # if lines detected, split the file name
    if(!is.na(pathLines)){
        script = script %>% {strsplit(.,"#")[[1]][1]}
    }
    assertthat::assert_that(!(!is.null(lines) & !is.na(pathLines)))
    text = RCurl::getURL(URLencode(paste0(
        "https://raw.githubusercontent.com/",user,'/',repo,'/master/',script)),
        ssl.verifypeer=FALSE)
    if (!is.na(pathLines)){
        lines = pathLines %>% stringr::str_extract_all("[0-9]+") %>% {.[[1]]}
        lines = seq(lines[1] %>% as.integer,lines[2] %>% as.integer, by = 1)
    }
    if(!is.null(lines)){
        text = text %>% strsplit(split = '\n') %>% {.[[1]]}
        text = text[lines] %>% paste(collapse='\n')
    }
    
    source(textConnection(text))
}



#' Load an Rdata file from a URL
#' @param url url of the Rdata file
#' @param envir the environment where the data should be loaded.
#' @return A character vector of the names of objects created, invisibly.
#' @export
loadURL = function(url,envir = parent.frame()){
    file = tempfile()
    download.file(url,destfile = file)
    y <- load(file, envir = envir)
}

#' Load an Rdata file from a URL
#' @param githubPath character. username/repository/pathToFile
#' @param branch which branch to source from
#' @return A character vector of the names of objects created, invisibly.
#' @param envir the environment where the data should be loaded.
#' @seealso \code{\link{loadURL}}, \code{\link{sourceGithub}}
#' @export
loadGithub = function(githubPath, branch = 'master', envir = parent.frame()){
    path = strsplit(githubPath,'/')[[1]]
    url = paste0("https://github.com/",
                 path[1],'/',path[2],'/blob/',branch,'/',
                 paste(path[3:length(path)], collapse = '/'),
                 '?raw=true')
    loadURL(url,envir)
}


#' @export
readRDSGithub = function(githubPath,branch = 'master'){
    path = strsplit(githubPath,'/')[[1]]
    readRDS(url(paste0("https://github.com/",
                       path[1],'/',path[2],'/blob/',branch,'/',
                       paste(path[3:length(path)], collapse = '/'),
                       '?raw=true')))
}
