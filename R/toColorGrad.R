toColorGrad <-
function(daList, startCol = 'white', endCol = 'red', fine = 0.01){
    colorGrad =  colorRampPalette(c(startCol, endCol))
    colorLegend = data.frame(value=seq(min(daList),max(daList),fine),
                             color=colorGrad(len(seq(min(daList),max(daList),fine))))
                             
    colors = colorLegend$color[
    sapply(daList,function(x){
        which(abs(colorLegend$value-x) == min(abs(colorLegend$value-x)))
    })]
    
    out = list(cols = colors, palette = colorLegend)
    return(out)
}
