##' @export
es_regression_old <- function(data,
                              yvar,
                              xvars,
                              tte_var,
                              fe = 0,
                              window = c(-9,9),
                              shift = 4,
                              dir.output = NULL,
                              dir.figs = NULL,
                              note = "ESPlot"){

  if (is.null(dir.output)){
    dir.output <- file.path(dir.root,'inst/output',Sys.Date())
    dir.create(dir.output)
  }

  dir.old = getwd()
  setwd(dir.output)

  if(is.null(dir.figs)){
    dir.figs <- file.path(".","ESfigs")
    dir.create(dir.figs)
  } else {
    dir.create(dir.figs)
  }

  sprintf(
    "%s ~ %s + %s | %s | 0 | 0",
    yvar,
    xvars %>>% paste(collapse = " + "),
    tte_var,
    fe %>>% paste(collapse = " + ")
  ) ->
    f

  message("Estimating the following models:")
  message(f)

  res <- list()
  data %>>%
    felm(
      formula = as.formula(f)
    ) %>>%
    extract_estimates %>>%
    attr('raw_estimates') ->
    res[['estimates']]

  res[['estimates']] %>>%
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
    ) ->
    dt2plot

  ## Set y = 0 at tte = 0 for all series

  dt2plot %>>%
    subset(
      x %between% window
    ) %>>%
    dplyr::arrange(x) %>>%
    dplyr::mutate(
      y = coef,
      y_cum = cumsum(coef)
    ) ->
    dt2plot2

  dt2plot2[x == -1][['y_cum']] ->
    correction

  dt2plot2 %>>%
    dplyr::mutate(
      y_cum_corr = y_cum - correction
    ) ->
    dt2plot3

  x.min <- dt2plot2 %>>% (x) %>>% min
  x.max <- dt2plot2 %>>% (x) %>>% max

  require(ggthemes)
  dt2plot3 %>>%
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
    p

  ## Save outputs
  figpath = file.path('.',
                      basename(dir.figs),
                      sprintf("ES_%s_yvar_%s_xvars_%s_fe_%s",
                              note,
                              yvar,
                              xvars %>>% paste(collapse = ","),
                              fe %>>% paste(collapse = ",")
                              ) %>>%
                        gsub(pattern = "[[:punct:]]",replacement = "") %>>%
                        gsub(pattern = "[[:space:]]",replacement = "") %>>%
                        sprintf(fmt = "%s.pdf"))
  ggsave(p,filename = figpath,height = 5, width = 10)


  data.table(
    yvar = yvar,
    xvars = xvars %>>% paste(collapse = ","),
    fe = fe,
    shift = shift,
    window = sprintf("[%s,%s]",window[1L],window[2L]),
    figpath = figpath,
    plot = sprintf("\\graph{1}{4}{%s}",figpath),
    note = note
  ) ->
    info

  setwd(dir.old)

  list(
    est = res[['estimates']],
    est_plot = dt2plot3,
    info = info
  ) ->
    output

  return(output)
}



