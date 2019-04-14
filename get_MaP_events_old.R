##' @export
get_MaP_events_old <- function(){

  c('all','quantity','price') %>>%
    list.map({
      loader(.) %>>%
        data_processor %>>%
        subset(tte_q == 0) %>>%
        dplyr::select(
          iso3,event.date,event.name,event.ix,event.diff
        )
    }) ->
    l.events
}

