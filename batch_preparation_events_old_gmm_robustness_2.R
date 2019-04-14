##' @export
batch_preparation_events_old_gmm_robustness_2  <- function(pre_qrt = 9,
                                             post_qrt = 9){
  ## -------------------------------------------------------------------------- ##
  ## COUNTRY CLASSIFICATIONS                                                    ##
  ## -------------------------------------------------------------------------- ##
  country_classification() ->
    ref.ctry

  ## -------------------------------------------------------------------------- ##
  ## LOAD EVENTS                                                                ##
  ## -------------------------------------------------------------------------- ##
  prepare_MaP_events_Cerutti2015_March_Robustness_2() %>>%
    rbindlist %>>%
    setnames(
      old = 'variable',
      new = 'type'
    )->
    events

  l.events <- list()

   ## INDEX FOR OLD EVENTS
  oldevents <- events %>>% copy
  oldevents %>>%
    arrange(type, iso3, date) %>>%
    (dt ~ dt[, event_index := {
      cumsum(chg.value)
    }, by = list(type,iso3,date)]) %>>%
    mutate(type = type %>>% as.character) %>>%
    select(type,iso3,date,event_index) ->
    dt_index_old

  date.min = "1995-01-01"
  date.max = "2016-06-01"

  ids = ref.ctry[['AE_EME.WB']][['iso3']] %>>% unique
  dates <- seq(
    from = date.min %>>% as.character %>>% as.Date,
    to = date.max %>>% as.character %>>% as.Date,
    by = '3 month'
  ) - 1
  CJ(iso3 = ids, date = dates, type = as.character(unique(dt_index_old[['type']]))) ->
    data

  dt_index_old %>>% mutate(date = date - 1) %>>% subset(!is.na(iso3)) %>>% setkey(type,iso3,date) ->
    dt_index_old

  data %>>% setkey(type,iso3,date) -> data

  (dt_index_old %>>% setkey(type,iso3,date))[
    data, roll = TRUE
    ] %>>%
    subset(!is.na(type)) %>>%
    subset(type %in% c('Price','Quantity')) %>>%
    (dt~dt[is.na(event_index), event_index := 0]) %>>%
    (dt~dt[,test := sum(event_index), by = list(type,iso3)]) %>>%
    subset(test != 0) %>>%
    select(-test) %>>%
    dcast(iso3+date ~ type) %>>%
    mutate(
      Price = ifelse(is.na(Price),0,Price),
      Quantity = ifelse(is.na(Quantity),0,Quantity),
      All = Price + Quantity
    )->
    event_index_old_events

  list(
    ref.ctry = ref.ctry,
    ## event_db = event_db,
    ## l.events = l.events,
    MPI = event_index_old_events
  ) ->
    out

  return(out)
}
