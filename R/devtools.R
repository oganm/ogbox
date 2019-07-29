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
#' @param newname Name of the resulting package. If NULL it'll default to packagename.versionnumber
#' @param token Github token if NULL GITHUB_PAT environment variable will be used
#' @param private,has_issues,has_wiki arguments to pass to gh
#' @export
forkCRAN = function(pkg, version = NULL, newname = NULL, token = NULL, private = FALSE, has_issues = FALSE, has_wiki = FALSE){
    if(!is.null(token)){
        old = Sys.getenv('GITHUB_PAT')
        Sys.setenv(GITHUB_PAT = token)
        on.exit(Sys.setenv(GITHUB_PAT = old))
    }
    cred = git2r::cred_token()
    
    available = available.packages()
    
    currentVersion = available[available[,'Package'] == pkg,"Version"]
    
    if(is.null(version)){
        version = currentVersion
    }
    
    if(is.null(newname)){
        name = paste0(pkg,'.',version)
    } else{
        name = newname
    }
    
    versionComp = compareVersion(currentVersion,version)
    
    newRepo = gh::gh('POST /user/repos',
           name = name,
           .token = token,
           private = private,
           has_issues = has_issues,
           has_wiki = has_wiki,
           auto_init = TRUE)
    
    tmp = tempfile()
    git2r::clone(newRepo$clone_url,local_path = tmp,credentials = cred)
    
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
    
    lines = readLines(glue::glue('{tmp}/DESCRIPTION'))
    lines[grepl('Package:',lines)] = paste0('Package: ',name)
    writeLines(lines,glue::glue('{tmp}/DESCRIPTION'))
    
    namespace = readLines(glue::glue('{tmp}/NAMESPACE'),encoding = 'UTF-8')
    namespace[grepl('useDynLib',namespace)] %<>% 
        gsub(glue::glue('useDynLib({pkg}'),
             glue::glue('useDynLib({name}'),
             x = .,fixed = TRUE)
    writeLines(namespace,glue::glue('{tmp}/NAMESPACE'))
    
    rFiles = list.files(glue::glue('{tmp}/R'),full.names = TRUE)
    rFiles %>% lapply(function(x){
        rfile = readLines(x,encoding = 'UTF-8')
        rfile[grepl('useDynLib',rfile)] %<>% 
            gsub(glue::glue('@useDynLib {pkg}'),
                 glue::glue('@useDynLib {name}'),
                 x = ., fixed = TRUE)
        writeLines(rfile,x)
    })
    
    
    files = list.files(tmp)
    git2r::add(tmp,path = files)
    git2r::commit(tmp,glue::glue('Copying version {version} of package {pkg} from CRAN'))
    git2r::push(tmp,credentials = cred)
}


#' Install generic remote package
#' 
#' Better version of the original by jimhester
#' https://github.com/r-lib/remotes/issues/383
#' @export
generic_install <- function(x,
                             dependencies = NA,
                             upgrade = c("default", "ask", "always", "never"),
                             force = FALSE,
                             quiet = FALSE,
                             build = TRUE, build_opts = c("--no-resave-data", "--no-manual", "--no-build-vignettes"),
                             build_manual = FALSE, build_vignettes = FALSE,
                             repos = getOption("repos"),
                             type = getOption("pkgType"),
                             ...) {
    # add cran:: to bare package names
    is_remote <- grepl("::|/", x)
    x[!is_remote] <- paste0("cran::", x[!is_remote])
    remotes <- lapply(x, remotes:::parse_one_remote, repos = repos, type = type)
    remotes:::install_remotes(remotes, dependencies = dependencies,
                              upgrade = upgrade, force = force, quiet = quiet,
                              build = build, build_opts = build_opts,
                              build_manual = build_manual, build_vignettes = build_vignettes,
                              repos = repos, type = type, ...)
}