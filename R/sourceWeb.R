#' Source R files from github
#' @description sources R files from github
#' @param githubPath character. username/repository/pathToFile
#' @param branch which branch to source from
#' @seealso \code{\link{loadURL}}, \code{\link{sourceGithub}}
#' @export
sourceGithub = function(githubPath, branch = 'master'){
    path = strsplit(githubPath,'/')[[1]]
    user = path[1]
    repo = path[2]
    script = paste(path[3:length(path)], collapse = '/')
    
    text = RCurl::getURL(paste0(
        "https://raw.githubusercontent.com/",user,'/',repo,'/master/',script),
        ssl.verifypeer=FALSE) 
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
