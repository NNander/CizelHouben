##' @export
summary_missing <- function(data,idcol = 'iso3',datecol = 'date'){
  data %>>%
    melt(
      id.vars = c(idcol,datecol)
    ) %>>%
    subset(
      !is.na(value)
    ) %>>%
    (dt~dt[, n_cs := {
      get(idcol) %>>% unique %>>% length
    }, by = c('variable')]) %>>%
    (dt~dt[, c('year_min',
               'year_max',
               'year_mean',
               'year_med') := {
                 x <- date %>>% year
                 list(
                   x %>>% min,
                   x %>>% max,
                   x %>>% mean,
                   x %>>% quantile(0.5)
                 )
               }, by = c('variable')]) %>>%
    setkey(variable) %>>%
    unique %>>%
    select(-iso3,-date,-value) %>>%
    arrange(year_min) ->
    dt

  data %>>%
    list.map({
      x <- .

      n.miss = is.na(x) %>>% sum
      n = x %>>% length
      missrate = n.miss/n

      data.table(
        name = .name,
        n = n,
        n.miss = n.miss,
        missrate = missrate
      )
    }) %>>%
  rbindlist %>>% arrange(missrate) -> dt2

  (dt %>>% setkey(variable))[
    dt2 %>>% setkey(name)
    ] %>>%
    arrange(missrate) ->
    o

  return(o)
}
