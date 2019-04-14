##' @export
extract_stats.felm <- function(o){
  o %>>% summary ->
    stats

  list(
    data.table(
      .ix = 'r2',
      .value = stats[['r2']] %>>% formatC(format = 'f', digits = 3)
    ),
    data.table(
      .ix = 'r2adj',
      .value = stats[['r2adj']] %>>% formatC(format = 'f', digits = 3)
    ),
    data.table(
      .ix = 'F-stat',
      .value = sprintf("F(%s,%s)=%s (%s)",
                       stats[['F.fstat']][['df1']] %>>% formatC(format = 'f', digits = 0),
                       stats[['F.fstat']][['df2']] %>>% formatC(format = 'f', digits = 0),
                       stats[['F.fstat']][['F']] %>>% formatC(format = 'f', digits = 2),
                       stats[['F.fstat']][['p']] %>>% formatC(format = 'f', digits = 3))
    ),
    data.table(
      .ix = 'N',
      .value = stats[['N']]
    )
  ) %>>% rbindlist ->
    out

  return(out)
}

##' @export
extract_stats.lrm <- function(o){

  data.table(
    .ix = o[['stats']] %>>% names,
    .value = o[['stats']] %>>% as.numeric %>>% formatC(format = 'f', digits = 3)
  ) ->
    part1


  data.table(
    .ix = sprintf("Num. of %s", o[['freq']] %>>% names),
    .value = sprintf("%s", o[['freq']])
  ) ->
    part2

  stat.sel = c('Obs','Num. of 1','Model L.R.','R2')

  list(
    part1,
    part2
  ) %>>%
    rbindlist %>>%
    subset(
      .ix %in% stat.sel
    ) %>>%
    mutate(
      .ix = .ix %>>% factor(levels = stat.sel)
    ) %>>%
    arrange(.ix) ->
    out

  return(out)
}
