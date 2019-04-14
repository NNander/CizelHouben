##' @export
batch_preparation_events_all_borrower <- function(pre_qrt = 12,
                                         post_qrt = 8){
  country_classification() ->
    ref.ctry

  imfutils::imfRefCtry() %>>%
    select(iso3,name) %>>%
    setkey(iso3) ->
    ref.iso3

  batch_preparation_events_old_borrower() ->
    l.event_data.old

  batch_preparation_events_new_borrower() ->
    l.event_data.new

  list(
    "New MaP dataset" = l.event_data.new,
    "Old MaP dataset" = l.event_data.old
  ) ->
    l.events

  return(l.events)
}


