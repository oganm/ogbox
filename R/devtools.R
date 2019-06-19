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

#' Make a github repository from a specific version of a pacakge
#' @param pkg Name of the package
#' @param version Desired version
#' @param private,has_issues,has_wiki arguments to pass to gh
#' @param credentials 
#' @export
forkOldCRAN = function(pkg, version, token = NULL, private = FALSE, has_issues = FALSE, has_wiki = FALSE){
    if(!is.null(token)){
        old = Sys.getenv('GITHUB_PAT')
        Sys.setenv(GITHUB_PAT = token)
        on.exit(Sys.setenv(GITHUB_PAT = old))
    }
    cred = git2r::cred_token()
    
    
    newRepo = gh::gh('POST /user/repos',
           name = paste0(pkg,'.',version),
           .token = token,
           private = private,
           has_issues = has_issues,
           has_wiki = has_wiki,
           auto_init = TRUE)
    
    tmp = tempfile()
    git2r::clone(newRepo$clone_url,local_path = tmp,credentials = cred)
    
    available = available.packages()
    
    currentVersion = available[available[,'Package'] == pkg,"Version"]
    
    versionComp = compareVersion(currentVersion,version)
    
    if(versionComp ==0){
        download.file(glue::glue('https://cran.r-project.org/src/contrib/{pkg}_{version}.tar.gz'),
                      destfile = glue::glue('{tmp}/pkgSource.tar.gz'))
    } else if(versionComp == 1){
        download.file(glue::glue('https://cran.r-project.org/src/contrib/Archive/{pkg}/{pkg}_{version}.tar.gz'),
                      destfile = glue::glue('{tmp}/pkgSource.tar.gz'))
    } else if(versionComp == -1){
        stop("that version doesn't exist yet")
    }
    
    untar(glue::glue('{tmp}/pkgSource.tar.gz'),exdir = tmp)
    files = list.files( glue::glue('{tmp}/{pkg}'),full.names = TRUE)
    file.copy(from = files,
              to = tmp,
              recursive = TRUE)
    file.remove(glue::glue('{tmp}/pkgSource.tar.gz'))
    unlink(glue::glue('{tmp}/{pkg}'),recursive = TRUE)
    
    files = list.files( glue::glue('{tmp}'))
    git2r::add(tmp,path = files)
    git2r::commit(tmp,glue::glue('Copying version {version} of package {pkg} from CRAN'))
    git2r::push(tmp,credentials = cred)
}