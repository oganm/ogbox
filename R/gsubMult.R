gsubMult <-
function(patterns, replacements, x,
                    ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE) {
    for (i in 1:length(patterns)){
        x = gsub(patterns[i],replacements[i],x,
                 ignore.case, perl, fixed, useBytes)
    }
    return(x)
}
