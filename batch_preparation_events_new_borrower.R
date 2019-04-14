##' @export
batch_preparation_events_new_borrower <- function(pre_qrt = 9,
                                         post_qrt = 9){
  country_classification() ->
    ref.ctry

  imfutils::imfRefCtry() ->
    ref.imf

  prepare_MaP_events_update() %>>%
    mutate(ifscode = as.character(ifscode)) %>>%
    mutate(variable = as.character(variable)) ->
    events.new

  eventsnew <- (ref.imf %>>%
    select(imfctry,iso3) %>>%
    setkey(imfctry))[
      events.new  %>>% setkey(ifscode)
      ] %>>%
    select(
      iso3,date,measure = variable,value
    ) %>>%
    mutate(
      type = Projects2016.Macropru::classify_price.vs.quantity(measure),
      type2 = classify_borrower.vs.other(measure)
    )

  eventsnew %>>%
    arrange(type, iso3, date) %>>%
    (dt ~ dt[, {
      list(event_index = sum(value))
    }, by = list(type2,iso3,date)]) %>>%
    (dt ~ dt[, event_index := {
      cumsum(event_index)
    }, by = list(type2,iso3)]) %>>%
    mutate(type = type2 %>>% as.character) %>>%
    select(type,iso3,date,event_index) %>>%
    subset(!is.na(iso3)) ->
    dt_index_new

  date.min = "1995-01-01"
  date.max = "2016-06-01"

  ids = ref.ctry[['AE_EME.WB']][['iso3']] %>>% unique
  dates <- seq(
    from = date.min %>>% as.character %>>% as.Date,
    to = date.max %>>% as.character %>>% as.Date,
    by = '3 month'
  ) - 1

  CJ(iso3 = ids, date = dates, type = as.character(unique(dt_index_new[['type']]))) ->
    data

  dt_index_new %>>% mutate(date = date - 1) %>>%subset(!is.na(iso3)) %>>% setkey(type,iso3,date) ->
    dt_index_new
  data <-  data %>>% setkey(type,iso3,date)


  (dt_index_new %>>% setkey(type,iso3,date))[
    data, roll = TRUE
    ] %>>%
    subset(!is.na(type)) %>>%
    (dt~dt[is.na(event_index), event_index := 0]) %>>%
    (dt~dt[,test := sum(event_index), by = list(type,iso3)]) %>>%
    subset(test != 0) %>>%
    select(-test) %>>%
    dcast(iso3+date ~ type) %>>%
    mutate(
      borrower = ifelse(is.na(borrower),0,borrower),
      institution = ifelse(is.na(institution),0,institution),
      All = borrower + institution
    )->
    event_index_new_events

  list(
    ref.ctry = ref.ctry,
    ## event_db = event_db,
    ## l.events = l.events,
    MPI = event_index_new_events
  ) ->
    out

  return(out)
}
