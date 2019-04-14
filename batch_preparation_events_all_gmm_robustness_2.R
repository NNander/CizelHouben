##' @export
batch_preparation_events_all_gmm_robustness_2 <- function(pre_qrt = 12,
                                         post_qrt = 8){
  country_classification() ->
    ref.ctry

  imfutils::imfRefCtry() %>>%
    select(iso3,name) %>>%
    setkey(iso3) ->
    ref.iso3

  batch_preparation_events_old_gmm_robustness_2() ->
    l.event_data.old

  batch_preparation_events_new_gmm_robustness_2() ->
    l.event_data.new

  list(
    "New MaP dataset" = l.event_data.new,
    "Old MaP dataset" = l.event_data.old
  ) ->
    l.events

  return(l.events)
}


