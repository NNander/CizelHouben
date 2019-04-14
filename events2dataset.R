##' @import lubridate
##' @export
events2dataset <- function(
                           events,
                           ids,
                           date.min = '1998-01-01',
                           date.max = '2014-12-31',
                           pre = 8,
                           post = 8,
                           idcol = 'iso3',
                           datecol = 'date',
                           keepcols = ""
                           ){
    events %>>% mutate(t = 1) ->
        events

    dates <- seq(
        from = date.min %>>% as.character %>>% as.Date,
        to = date.max %>>% as.character %>>% as.Date,
        by = '3 month'
    ) - 1

    CJ(ids, dates) %>>%
        setnames(
            old = names(.),
            new = c('idref',
                    'date')
        ) ->
        data

    ## Add events
    (events %>>% setkeyv(c(idcol,datecol)))[data %>>% setkeyv(c('idref','date'))] %>>%
      (dt ~ dt[is.na(t), t:=0]) ->
      data2

    ## ---------------------------------------------------------------------- ##
    ## Events 2 panel format                                                  ##
    ## ---------------------------------------------------------------------- ##
    data2 %>>%
      select(one_of(idcol,datecol,keepcols), dum_event = t) %>>%
      ## subset(dum_event > 0)
      (dt~dt[,{
        .SD %>>%
          select(
            one_of(keepcols)
          ) %>>%
          lapply(
            function(x) {
              x %>>%
                as.character %>>%
                paste(collapse = ";") ->
                x
              ifelse(x == 'NA','',x)
            }
          ) ->
          o
        .SD %>>%
          (dum_event) %>>%
          sum ->
          o[['.dum_event']]
        o
      },by = c(idcol,datecol)]) %>>%
      (dt~dt[, .dum_event.id := rleid(.dum_event), by = c(idcol)]) %>>%
      (dt~dt[, .dum_event.t := 1:.N, by = c(idcol,'.dum_event.id')]) ->
      event_panel

    ## RESHAPING
    data2 %>>% .raw2processed(pre = pre,
                              post = post,
                              idcol = idcol,
                              datecol = datecol,
                              keepcols = keepcols) ->
      out

    out[['raw_events']] <- events
    out[['event_panel']] <- event_panel
    attr(out, 'class') <- c(class(out),'events2dataset')

    return(out)


## Split to TS:
.raw2processed <- function(events,
                           pre = 8,
                           post = 8,
                           idcol = 'iso3',
                           datecol = 'date',
                           keepcols = NULL){

    events[, idref := get(idcol)]

    events %>>%
        split(. %>>% (idref)) ->
        l

    l %>>%
        list.map({
            ## message(.name)
            . %>>%
                .Apply2TS(pre = pre,
                          post = post,
                          idcol = idcol,
                          datecol = datecol,
                          keepcols = keepcols)
        }) ->
         o

    ## Build treatment
    o %>>%
        list.map({
            .[['treatment']]
        }) %>>%
         Filter(f = function(x) !is.null(x)) %>>%
         rbindlist ->
         df_treat

    ## Build control
    o %>>%
        list.map({
            .[['control']]
        }) %>>%
         Filter(f = function(x) !is.null(x)) %>>%
         rbindlist ->
         df_ctrl

    df_ctrl %>>%
        split(.[['.ix']]) %>>%
        list.map({
            . %>>%
                .convertInfo2TS(datecol = datecol)
        })  %>>%
         rbindlist ->
         data_ctrl

    df_treat %>>%
        split(.[['.ix']]) %>>%
        list.map({
            . %>>%
                .convertInfo2TS(datecol = datecol)
        })  %>>%
         rbindlist ->
         data_treat

    return(list(
        control = data_ctrl,
        treatment = data_treat,
        control_lookup = df_ctrl,
        treatment_lookup = df_treat
    ))
}



.convertInfo2TS <- function(info,datecol){
    dates <- seq(
        from = info[['begin']] + 1,
        to = info[['end']] + 1,
        by = '3 month'
    ) - 1

    data.table(
        info %>>% select(-one_of(datecol)) %>>% mutate(event_date = mid),
        date = dates
    ) %>>%
        mutate(
            tte = round_any(((date - event_date) %>>% as.numeric) / 365, 0.25)
        ) ->
        out

    return(out)
}


## ts %>>% .Apply2TS(pre = 8, post = 8) ->
##     o

## For each TS check:
.Apply2TS <-
    function(
             ts,
             pre = 6,
             post = 3,
             idcol = 'iso3',
             datecol = 'date',
             keepcols = ""
             ){
    by = 1

    ts %>>%
        (t) %>>%
        rollapply(
            width = pre,
            by = by,
            FUN = function(x){
                1*((x==1) %>>% any)
            }
        ) %>>%
        (c(.,rep(NA,pre-by))) ->
        t_pre

    ts %>>%
        (t) %>>%
        rev %>>%
        rollapply(
            width = post,
            by = by,
            FUN = function(x){
                1*((x==1) %>>% any)
            }
        ) %>>%
        (c(.,rep(NA,post-by))) %>>%
        rev ->
        t_post

    ts %>>%
        mutate(
            t_pre = t_pre,
            t_post = t_post,
            t_test = 1*(t_pre | t_post)
        ) ->
        ts2

    ts2 %>>%
        subset(
            t_test == 1
        ) ->
        ts_treat

    ts2 %>>%
        subset(
            t_test == 0
        ) ->
        ts_control

    ## Mark usable portions of ts_control
    ## Usable means that there should be pre+post-1 consecutive observations

    if (NROW(ts_control) < pre + post - 1){
        if(NROW(ts_control) == 0){
            ts_control2 = ts_control
        } else {
            ts_control %>>%
                mutate(
                    avail = FALSE
                ) ->
                ts_control2
        }
    } else {
        ts_control %>>%
            (date) %>>%
            rollapply(
                width = pre + post - 1,
                by = by,
                FUN = function(x){
                    (seq(
                        from = x %>>% min + 1,
                        to = x %>>% max + 1,
                        by = '3 month'
                    ) - 1) %>>%
                        length %>>%
                        (. == (pre + post - 1))
                }
            ) %>>%
            (c(.,rep(NA,pre+post-by-1)))  ->
            avail

        ts_control %>>%
            mutate(
                avail = avail
            ) ->
            ts_control2
    }

    if(NROW(ts_control) == 0){
        ts_control3 = NULL
    } else {
        ts_control2 %>>%
            subset(
                avail == TRUE
            ) %>>%
            (dt~dt[,.ix := sprintf("C.%s.%s",
                                   get(idcol),1:.N)]) %>>%
            select(
                one_of(idcol),
                one_of(datecol),
                .ix,
                one_of(keepcols)
            ) %>>%
            mutate(
                begin = date,
                mid = (date + 1 + pre * months(3)) - 1,
                end = (date + 1 + (pre + post - 1) * months(3)) - 1
            ) ->
            ts_control3
    }

    ## Treatment
    ts %>>%
        subset(t == 1) %>>%
        (dt~dt[,.ix := sprintf("T.%s.%s",
                                    get(idcol),1:.N)]) %>>%
        select(
            one_of(idcol),
            one_of(datecol),
            .ix,
            one_of(keepcols)
        ) %>>%
        mutate(
            begin = (date + 1 - pre * months(3)) - 1,
            mid = date,
            end = (date + 1 + (post - 1) * months(3)) - 1
        )  ->
        ts_treatment

        return(list(
            control = ts_control3,
            treatment = ts_treatment
        ))
}




