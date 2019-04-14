#' @export
simulate_events <- function(edata){
  CJ(
    iso3 = edata[['iso3']] %>>% unique,
    date = seq(
      from  = '2000-01-01' %>>% as.Date,
      to = '2016-03-01' %>>% as.Date,
      by = 'months'
    ) - 1,
    type = c('Quantity','Price')
  ) ->
    SKELETON


  edata %>>%
    subset(type != "All") %>>%
    (dt~dt[, {
      list(
        .n.events = .N
      )
    }, by = type]) ->
    EVENT_NUM

  (EVENT_NUM %>>% setkey(type))[
    SKELETON %>>% setkey(type)
    ] %>>%
    (dt ~ dt[, event := {
      rbinom(
        n = .N,
        size = 1,
        p = .n.events/.N
      )
    }, by = type]) %>>%
    subset(
      event == 1
    ) %>>%
    dplyr::select(
      iso3, date, type
    ) ->
    SIMULATED

  return(SIMULATED)
}
