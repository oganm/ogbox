sourceGithub <-
function(user, repo, script){
    user = substitute(user)
    user = as.character(user)
    repo = substitute(repo)
    repo = as.character(repo)
    script = substitute(script)
    script = as.character(script)
    
    require(RCurl)
    if (!grepl('[.](r|R)',script)){
        script = paste0(script,'.R')
    }
    text = getURL(paste0(
        "https://raw.githubusercontent.com/",user,'/',repo,'/master/',script),
        ssl.verifypeer=FALSE) 
    source(textConnection(text))
}
