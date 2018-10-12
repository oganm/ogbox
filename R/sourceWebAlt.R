#' Load RDA file from github.
#' 
#' Loads data from github. Allows authentication. Max file size is 1 MB
#' 
#' @export
loadGithubAuth = function(githubPath, branch = 'master', envir = parent.frame(), token = NULL){
    path = strsplit(githubPath,'/')[[1]]
    
    url = glue::glue('https://api.github.com/repos/{path[1]}/{path[2]}/contents/',
                     paste(path[3:length(path)], collapse = '/'))
    
    ghFile = gh::gh('GET /repos/:username/:reponame/contents/:path',
                    username = path[1],
                    reponame = path[2],
                    path = paste(path[3:length(path)], collapse = '/'),
                    .token = token)
    url = ghFile$download_url
    loadURL(url,envir)
}



#' Source R files from github.
#' @description sources R files from github. Allows authentication. Max file size is 1 MB
#' @param githubPath character. username/repository/pathToFile. Lines to excecute can be provided with github's #L1-L5 syntax. The function doesn't work with files that have ? # [ or ] in their addresses.
#' @param line numbers to excecute. Providing lines in both here and with githubPath will result in an error.
#' @param branch which branch to source from
#' @seealso \code{\link{loadURL}}, \code{\link{sourceGithub}}
#' @export
sourceGithubAuth = function(githubPath,lines = NULL, branch = 'master', token = NULL){
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
    
    ghFile = gh::gh('GET /repos/:username/:reponame/contents/:path',
                    username = user,
                    reponame = repo,
                    path = script,
                    .token = token)
    
    url = ghFile$download_url
    
    text = RCurl::getURL(url,
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