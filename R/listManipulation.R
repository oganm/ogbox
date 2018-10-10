#merges lists by their common names. adds non common ones.
#' @export
mergeList = function(aList,bList,forceUnique=T){
    allNames = unique(c(names(aList),names(bList)))
    outList = vector(mode= "list",length = length(allNames))
    names(outList) = allNames
    outList = sapply(allNames,function(x){
        out=(c(aList[[x]],bList[[x]]))
        if (forceUnique){
            out = unique(out)
        }
        return(out)
    })
    return(outList)
}

# seeks for a given object in a single layered list
#' @export
findInList = function(object, list){

    indices = vector()
    for (i in 1:length(list)){
        if (object %in% list[[i]]){
            indices = c(indices, i)
        }
    }
    return(indices)
}

# counts total no of elements in a single layered list
#' @export
listCount = function(aList){
    length(unlist(aList))
}


# vectorized and generalized version of find in list
#' @export
findInDeepList = function(objects, list){
    findTRUE = function(i,x){
        if(class(x[[i]]) %in% 'list'){
            out = lapply(1:length(x[[i]]),function(j){
                y = findTRUE(j,x[[i]])
            })
            names(out) = 1:length(x[[i]])
            out = names(out) %>% {.[sapply(out,len)>0]} %>% lapply(function(y){paste0(y,', ',out[[y]])}) %>% unlist
            return(out)
        } else{
            which(x[[i]])
        }
    }
    
    unlisted = unlist(list)
    logicalList = lapply(objects, function(obj){
        unlisted %in% obj
    }) %>% lapply(relist,skeleton = list)
    
    
    logicalList %>% lapply(function(x){
        out = 1:length(x) %>% lapply(findTRUE,x)
        names(out) = 1:length(x)
        out = names(out) %>% {.[sapply(out,len)>0]} %>% lapply(function(y){paste0(y,', ',out[[y]])}) %>% unlist
        return(out)
    }) %>% lapply(strsplit,', ') %>% sapply(function(x){
        x %>% lapply(as.integer)
    })
}

# counts total no of elements in a single layered list
#' @export
listCount = function(aList){
    length(unlist(aList))
}


# finds total depth of a list which is assumed to be symmetrical
#' @export
listDepth = function(deList){
    step = 1
    while (T){
        if (typeof(eval( parse(text = paste(c("deList",rep('[[1]]',step)),sep='',collapse = '')))) != "list"){
            return(step)
        }
        step = step +1
    }
}


# for navigating through list of lists with teval
#' @export
listParse = function (daList,daArray){
    out = listStr(daArray)
    teval(paste0('daList' , out))
}

#returns the final step as a list
#' @export
listParseW = function (daList,daArray){
    out = listStrW(daArray)
    teval(paste0('daList' , out))
}



# sets the list element
#' @export
listSet = function(daList,daArray ,something){
    name = substitute(daList)
    name = as.character(name)
    out = listStr(daArray)
    teval(paste0(name, out, '<<-something'))
}



# > listStr(c(1,2,3))
# [1] "[[1]][[2]][[3]]"
#' @export
listStr = function(daArray){
    out = ''
    for (i in daArray[1 : length(daArray)]){
        
        out = paste0(out, '[[',  i, ']]')
    }
    return(out)
}

# the last element is returned with a singe "["
# > listStrW(c(1,2,3))
# [1] "[[1]][[2]][3]"
#' @export
listStrW = function(daArray){
    out = ''
    if (length(daArray) > 1){
        for (i in daArray[1 : (length(daArray) - 1)]){
            out = paste0(out,'[[',i, ']]')
        }
    }
    out = paste0(out,'[', daArray[length(daArray)],']')
    return(out)
}



# http://stackoverflow.com/questions/18122548/display-names-of-column-of-recursive-list-as-tree
# displays a list as a tree by their names
#' @export
nametree <- function(X, prefix1 = "", prefix2 = "", prefix3 = "", prefix4 = "")
    if( is.list(X) )
        for( i in seq_along(X) ) { 
            cat( if(i<length(X)) prefix1 else prefix3, names(X)[i], "\n", sep="" )
            prefix <- if( i<length(X) ) prefix2 else prefix4
            nametree(
                X[[i]], 
                paste0(prefix, "├──"),
                paste0(prefix, "│  "),
                paste0(prefix, "└──"),
                paste0(prefix, "   ")
            )
        }



#' @export
nametreeVector <- function(X, prefix1 = "", prefix2 = "", prefix3 = "", prefix4 = ""){
    out = NULL
    if( is.list(X) ){
        out = vector(mode='list',length = length(X))
        for( i in seq_along(X) ) { 
            out[[i]] = paste0( if(i<length(X)) prefix1 else prefix3, names(X)[i], "\n", sep="" )
            prefix <- if( i<length(X) ) prefix2 else prefix4
            out2 = nametreeVector(
                X[[i]], 
                paste0(prefix, "├──"),
                paste0(prefix, "│  "),
                paste0(prefix, "└──"),
                paste0(prefix, "\U00A0\U00A0\U00A0\U00A0")
            )
            out[[i]] = c(out[[i]],out2)
        }
    }
    return(unlist(out))
}


#' Transform a data frame into a nested list.
#' @description Takes in given columns of a data frame and transforms them into a nested list based on the column names 
#' given. Eg. if first level is \code{c(1,1,2)} and second level is \code{c('a','b','c')}, it will return a list of 
#' length 2 that includes lists named 1 and 2. List 1 will include 2 empty lists named a and b while list 2 will include
#' 1 empty list called c.
#' @return A nested list
#' @export
frame2tree = function(frame, levels= colnames(frame)){
    out = vector(mode = 'list', length = len(unique(frame[levels[1]]) %>% trimNAs))
    
    out = lapply(out,function(x){list()})
    names(out) = unique(frame[levels[1]]) %>% trimNAs %>% sort
    
    if ((len(levels)>1) & (nrow(frame)>0)){
        out = lapply(names(out),function(x){
            frame2tree(frame[frame[,levels[1]] %in% x,], levels[-1] )
        })
        names(out) = unique(frame[levels[1]]) %>% trimNAs %>% sort
        for(i in 1:len(out)){
            if (len(out[[i]])==1 && names(out[[i]]) == names(out[i])){
                out[[i]] = list()}
        }
    }
    return(out)
}
