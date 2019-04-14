##' @export
batch_preparation_events_all <- function(pre_qrt = 12,
                                         post_qrt = 8){
  country_classification() ->
    ref.ctry

  imfutils::imfRefCtry() %>>%
    select(iso3,name) %>>%
    setkey(iso3) ->
    ref.iso3

  batch_preparation_events_old(pre_qrt = pre_qrt,
                               post_qrt = post_qrt) ->
    l.event_data.old

  batch_preparation_events_new(pre_qrt = pre_qrt,
                               post_qrt = post_qrt) ->
    l.event_data.new

  list(
    "New MaP dataset" = l.event_data.new,
    "Old MaP dataset" = l.event_data.old
  ) ->
    l.events

  l.events %>>%
    list.map({
      l.old = .

      event_db[c('treatment_lookup','control_lookup')] %>>%
        rbindlist ->
        elookup

      (ref.iso3 %>>% setkey(iso3))[
        elookup %>>% setkey(iso3)
        ] %>>%
        mutate(
          .print. = sprintf("%s (%s %s)",
                            name,
                            date %>>% month %>>% as.numeric %>>% (x ~ month.abb[x]),
                            date %>>% year)
        ) %>>%
        dplyr::select(
          .ix,
          iso3,
          country = name,
          edate = date,
          type,
          .print.
        ) ->
        l.old[['Event Lookup']]

      .[['l.events']] ->
        l.old[['Clean overlapping events']]

      l.old
    }) -> l.events

  return(l.events)
}


