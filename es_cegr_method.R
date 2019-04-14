##' @export
es_cegr_method <-
  function(data,
           yvar,
           xvars,
           tte_var,
           fe = 0,
           window = c(-9,9),
           shift = 4)
  {
    sprintf(
      "%s ~ %s + %s | %s | 0 | 0",
      yvar,
      xvars %>>% paste(collapse = " + "),
      tte_var,
      fe %>>% paste(collapse = " + ")
    ) ->
      formula

    message("Estimating the following model:")
    message(formula)

    l.out <- list()
    data %>>%
      felm(
        formula = as.formula(formula)
      ) ->
      o

    o ->
      l.out[['FELM object']]

    o %>>%
      extract_estimates %>>%
      attr('raw_estimates') ->
      l.out[['estimates']]

    try({
      o %>>%
        wald_test(shift = shift,tte_pattern = Hmisc::escapeRegex(tte_var))
    }) ->
      l.out[['waldtest']]

    l.out[['estimates']] %>>%
      dplyr::mutate(
        x = gsub(pattern = Hmisc::escapeRegex(tte_var),
                 replacement = "",
                 .ix) %>>%
          as.numeric %>>%
          (z ~ z + shift)
      ) %>>%
      dplyr::arrange(
        x
      ) %>>%
      subset(
        !is.na(x)
      ) ->>     #adjusted >>
      l.out[['plotting data']]

    if (min(l.out[['plotting data']][['x']]) >= 0){
      l.out[['plot']] <- NULL
    } else {
      ## Set y = 0 at tte = 0 for all series
      l.out[['plotting data']] %>>%
        subset(
          x %between% window
        ) %>>%
        dplyr::arrange(x) %>>%
        dplyr::mutate(
          y = coef,
          y_cum = cumsum(coef)
        ) ->
        .tempdt

      .tempdt[x == 0][['y_cum']] ->
        correction

      .tempdt %>>%
        dplyr::mutate(
          y_cum_corr = y_cum - correction
        ) ->
        .tempdt

      x.min <- .tempdt %>>% (x) %>>% min
      x.max <- .tempdt %>>% (x) %>>% max

      ## PLOTTER
      require(ggthemes)
      .tempdt %>>%
        ggplot(
          aes(
            x = x,
            y = y_cum_corr
          )
        ) +
        geom_line() +
        ggthemes::theme_tufte() +
        geom_vline(xintercept = -4,linetype = 'dashed') +
        geom_vline(xintercept = +4,linetype = 'dashed') +
        geom_vline(xintercept = -8,linetype = 'dashed') +
        geom_vline(xintercept = +8,linetype = 'dashed') +
        geom_vline(xintercept = 0) +
        geom_hline(yintercept = 0) +
        scale_x_continuous(breaks = x.min:x.max) +
        labs(
          x = 'Quarters-to-Event',
          y = ''
        ) ->
        ## theme(plot.caption=element_text(size=9, hjust=0, margin=margin(t=1)))
        l.out[['plot']]
    }

    data.table(
      yvar = yvar,
      xvars = xvars %>>% paste(collapse = ","),
      fe = fe,
      shift = shift,
      window = sprintf("[%s,%s]",window[1L],window[2L]),
      felm.obj = list(l.out[['FELM object']]),
      felm.estimates = list(l.out[['estimates']]),
      felm.wald = list(l.out[['waldtest']]),
      plotting_data = list(l.out[['plotting data']]),
      plot = list(l.out[['plot']])
    ) ->
      OUTPUT

    return(OUTPUT)
  }



