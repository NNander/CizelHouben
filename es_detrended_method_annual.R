##' @export
##' @import ggplot2 scales
es_detrended_method_annual <- function(data,
                                idcol = '.ix',
                                tte_var = 'tte_shift',
                                value_var = 'value'){

  data %>>%
    copy %>>%
    mutate(
      .id. = get(idcol),
      .tte. = get(tte_var),
      .value. = get(value_var)
    ) ->
    data

  result = list()
  data %>>%
    (dt ~ dt[, {
      .N. = .N
      .egr. = .value. %>>%
        statar::winsorise(probs=c(0.01,0.99), verbose = FALSE) %>>%
        mean(na.rm = TRUE)
      .egr.sd. = .value. %>>%
        statar::winsorise(probs=c(0.01,0.99), verbose = FALSE) %>>%
        sd(na.rm = TRUE)
      .egr.zstat. = .egr. / (.egr.sd./sqrt(.N.))
      .egr.pval. = 2 * pt(-abs(.egr.zstat.), df = Inf)

      list(
        .N. = .N,
        .egr. = .egr.,
        .egr.sd. = .egr.sd.,
        .egr.zstat. = .egr.zstat.,
        .egr.pval. = .egr.pval.
      )
    }, by = c('.tte.')]) ->
  result[['Event study result by TTE']]

  list(
    "[-8,-1]" = c(-3,-1),
    "[-4,-1]" = c(-2,-1),
    "[0,4]" = c(0,1),
    "[0,8]" = c(0,2)
  ) ->
    l.intervals

  l.intervals %>>%
    list.map({
      INTERVAL_NAME = .name
      INTERVAL = .
      data %>>%
        ## subset(
        ##   .tte. %between% INTERVAL
        ## ) %>>%
        (dt ~ dt[, {
          .cegr. = (.SD[.tte. == INTERVAL[2L]][['.value.']] - .SD[.tte. == (INTERVAL[1L] - 1)][['.value.']])
          list(
            .cegr. = .cegr.
          )
        }, by = '.id.']) ->
        data_cegr

      .N. = data_cegr %>>% (.cegr.) %>>% length
      .cegr. = data_cegr %>>% (.cegr.) %>>%
        statar::winsorise(probs=c(0.01,0.99), verbose = FALSE) %>>%
        mean(na.rm = TRUE)
      .cegr.sd. = data_cegr %>>% (.cegr.) %>>%
        statar::winsorise(probs=c(0.01,0.99), verbose = FALSE) %>>%
        sd(na.rm = TRUE)
      .cegr.zstat. = .cegr. / (.cegr.sd./sqrt(.N.))
      .cegr.pval. = 2 * pt(-abs(.cegr.zstat.), df = Inf)
      .print. = sprintf("%s%s (p=%s)",
                        (100*.cegr.) %>>% formatC(format = 'f',digits = 2),
                        .cegr.pval. %>>% stars,
                        .cegr.pval. %>>% formatC(format = 'f',digits = 2)
                        )

      data.table(
        interval = INTERVAL_NAME,
        .N. = .N.,
        .cegr. = .cegr.,
        .cegr.sd. = .cegr.sd.,
        .cegr.zstat. = .cegr.zstat.,
        .cegr.pval. = .cegr.pval.,
        .print.
      )
    }) %>>%
  rbindlist -> result[['CEGR results by window']]

  result[['CEGR results by window']] %>>%
    mutate(
      notes = sprintf("CEGR%s = %s",interval,.print.)
    ) %>>%
    (notes) %>>%
    paste(collapse = "; ") %>>%
    stringr::str_wrap(80) ->
    caption

  ## PLOT
  result[['Event study result by TTE']] %>>%
    subset(.tte. %between% c(-3,2)) %>>%
    ggplot(
      aes(
        x = .tte.,
        y = .egr.
      )
    ) +
    geom_line() +
    theme_tufte() +
    geom_vline(xintercept = 0, size = 0.2) +
    geom_hline(yintercept = 0, size = 0.2) +
    scale_x_continuous(breaks = -8:8) +
    scale_y_continuous(labels = percent) +
    ## theme(legend.position="bottom") +
    labs(
      x = 'Quarters-to-event',
      y = ''
      ## caption = caption
    ) ->
    ## theme(plot.caption=element_text(size=9, hjust=0, margin=margin(t=1))) ->
    result[['plot']]

  return(result)
}

