##' @export
summary_allmissing <- function(data,idcol = 'iso3',datecol = 'date', sort = TRUE, xcolIsIdcol = TRUE){
  if (xcolIsIdcol == TRUE){
    xcol = idcol
    ycol = 'variable'
  } else {
    xcol = 'variable'
    ycol = idcol
  }

  data %>>%
    melt(
      id.vars = c(idcol,datecol)
    ) %>>%
    (dt ~ dt[,{
      list(allmissing = all(is.na(value)))
    }, by = c('variable',idcol)]) %>>%
    subset(
      allmissing == TRUE
    ) %>>%
    ## Report number of all missing vars by country
    (dt ~ dt[, nmiss.byid := {
      sum(allmissing)
    }, by = c(idcol)]) %>>%
    ## Report number of all missing countries by variable
    (dt ~ dt[, nmiss.byvar := {
      sum(allmissing)
    }, by = list(variable)]) %>>%
    mutate(
      allmissing.sig = ifelse(allmissing==TRUE,
                              "X",
                              "")
    ) ->
      o

  o %>>%
    dcast(
      sprintf("%s ~ %s", xcol,ycol),
      value.var = 'allmissing.sig',
      fill = ''
    ) ->
    .out.1

  o %>>%
    select(one_of(idcol),nmiss.byid) %>>%
    setkeyv(idcol) %>>%
    unique %>>%
    arrange(-nmiss.byid)->
    .out.2

  o %>>%
    select(variable,nmiss.byvar) %>>%
    setkey(variable) %>>%
    unique %>>%
    arrange(-nmiss.byvar)->
    .out.3

  if (sort == TRUE){
    if (xcolIsIdcol == TRUE){
      .out.1 %>>%
        mutate(
          id = get(idcol) %>>% factor(levels = .out.2[[idcol]])
        ) %>>%
        arrange(
          id
        ) %>>%
        select(
          id, one_of(.out.3[['variable']] %>>% as.character)
        ) ->
        out.final
    } else {
      .out.1 %>>%
        mutate(
          id = variable %>>% factor(levels = .out.3[['variable']])
        ) %>>%
        arrange(
          id
        ) %>>%
        select(
          id, one_of(.out.2[[idcol]] %>>% as.character)
        ) ->
        out.final
    }
  } else {
    out.final = .out.1
  }

  attr(out.final,'nmiss.byid') <- .out.2
  attr(out.final,'nmiss.byvar') <- .out.3
  return(out.final)
}
