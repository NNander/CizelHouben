##' @export
event_dataset_preparation <- function(data,ids,date.min = '1995-01-01',date.max = '2016-06-01',pre_qrt = 9, idcol = 'iso3', datecol = 'date', post_qrt = 9, keepcols = "",tte_window = c(-16,12)){
  data %>>%
    eventstudy::events2dataset(
      ids = ids,
      date.min = date.min,
      date.max = date.max,
      pre = pre_qrt,
      post = post_qrt,
      idcol = idcol,
      datecol = datecol,
      keepcols = keepcols
    ) ->
    event_db

  event_db[['event_panel']] ->
    event_panel

  event_db[['treatment_lookup']] %>>%
    copy %>>%
    (dt ~ dt[, N.events := .N, by = idcol]) %>>%
    select(one_of(idcol),N.events) %>>%
    setkeyv(idcol) %>>%
    unique ->
    treated_ncases

  (treated_ncases %>>%
     setkeyv(idcol))[
       event_panel %>>%
         setkeyv(idcol)
       ] %>>%
    dplyr::select(one_of(c(idcol,datecol)),N.events) %>>%
    dplyr::mutate(treated = ifelse(is.na(N.events), FALSE, TRUE)) %>>%
    (dt~dt[is.na(N.events),N.events:=1]) %>>%
    (dt~dt[rep(seq_len(nrow(dt)),times = N.events)]) %>>%
    (dt~dt[,.ix := sprintf("%s.%s.%s",
                           ifelse(treated == TRUE, 'T','C'),
                           get(idcol),
                           1:.N), by = c(idcol,datecol)])->
    event_panel_expanded

  (event_db[['treatment_lookup']] %>>%
     select(-one_of(c(idcol,datecol))) %>>%
     setkey(.ix))[
       event_panel_expanded %>>%
         setkey(.ix)
       ] %>>%
    setnames(
      old = 'mid',
      new = 'event.date'
    ) %>>%
    dplyr::select(-begin,-end) %>>%
    arrange(.ix, date) %>>%
    mutate(
      tte = 4*(plyr::round_any(as.numeric(date - event.date) / 365, 0.25))
    ) %>>%
    mutate(
      tte = ifelse(is.na(tte),-999,
                   ifelse(
                     tte %between% tte_window,
                     tte,
                     -999
                   ))
    ) ->
    event_panel_expanded2

  event_db[['control']] %>>%
    dplyr::select(one_of(c(idcol,datecol))) %>>%
    setkeyv(c(idcol,datecol)) %>>%
    unique %>>%
    dplyr::mutate(
      control = TRUE
    ) ->
    control_cases

  (control_cases %>>%
     setkeyv(c(idcol,datecol)))[
       event_panel_expanded2 %>>%
         setkeyv(c(idcol,datecol))
       ] %>>%
    mutate(
      control = ifelse(is.na(control),FALSE,control)
    ) %>>%
    subset(
    (control == TRUE) | (tte != -999)
    ) %>>%
    setkeyv(c(idcol,datecol,'.ix')) ->
    event_panel_expanded3

  event_db[['event_panel_expanded']] <- event_panel_expanded3

  return(event_db)
}
