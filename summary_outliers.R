## summary

## sheet = 'Variable distributions'
## try(removeWorksheet(wb,sheet))
## addWorksheet(wb,sheet)

## dev.off()
## yxdata.sel2 %>>%
##   melt(
##     id.vars = c('iso3','date')
##   ) %>>%
##   subset(!is.na(value)) %>>%
##   (dt ~ dt[,{
##     var <- .BY[[1L]] %>>% as.character
##     message(var)
##     par(mar = rep(2, 4))
##     value %>>% hist(breaks = 50, main = var)
##     insertPlot(wb,sheet,startRow = .GRP * 20)
##     dev.off()
##   }, by = 'variable'])
