countWords = function(){
    context = rstudioapi::getActiveDocumentContext()
    context$selection[[1]]$text %>% 
        stringr::str_split(' ') %>% {.[[1]]} %>% 
        {.[.!='']} %>% length -> out
    
    rstudioapi::showDialog("Word count in selection",message = out)
}

countCharacters = function(){
    context = rstudioapi::getActiveDocumentContext()
    context$selection[[1]]$text %>% nchar ->out
    
    rstudioapi::showDialog("Character count in selection",message = out)
}