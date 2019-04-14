##' @import lfe gdata
##' @export
wald_test <- function(o, shift = 0, tte_pattern = "factor\\(tte\\)"){
  l <- list()
  x <- coef(o)
  o %>>% testprep(shift = shift,tte_pattern=tte_pattern) ->
    coef_df

  window2coefix <- function(window){
    coef_df[tte_shift %between% window][['coef.ix']]
  }

  ## ------------------------------------------------------------------------ ##
  ## CEGR 2Y PRE                                                              ##
  ## ------------------------------------------------------------------------ ##
  .name. = 'CEGR[-8,-1]'
  window = c(-8,-1)
  rep(0, times = length(x)) ->
    l[[.name.]]
  l[[.name.]][window2coefix(window)] <- 1

  ## ------------------------------------------------------------------------ ##
  ## CEGR 1Y PRE                                                              ##
  ## ------------------------------------------------------------------------ ##
  .name. = 'CEGR[-4,-1]'
  window = c(-4,-1)
  rep(0, times = length(x)) ->
    l[[.name.]]
  l[[.name.]][window2coefix(window)] <- 1

  ## ------------------------------------------------------------------------ ##
  ## CEGR 2Y POST                                                             ##
  ## ------------------------------------------------------------------------ ##
  .name. = 'CEGR[0,8]'
  window = c(0,8)
  rep(0, times = length(x)) ->
    l[[.name.]]
  l[[.name.]][window2coefix(window)] <- 1

  ## ------------------------------------------------------------------------ ##
  ## CEGR 1Y POST                                                             ##
  ## ------------------------------------------------------------------------ ##
  .name. = 'CEGR[0,4]'
  window = c(0,4)
  rep(0, times = length(x)) ->
    l[[.name.]]
  l[[.name.]][window2coefix(window)] <- 1


  ## ------------------------------------------------------------------------ ##
  ## DID[2Y pre - 2Y post]                                                    ##
  ## ------------------------------------------------------------------------ ##
  l[['CEGR[0,8]']] - l[['CEGR[-8,-1]']] ->
    l[['DID[2 year]']]

  ## ------------------------------------------------------------------------ ##
  ## DID[1Y pre - 1Y post]                                                    ##
  ## ------------------------------------------------------------------------ ##
  l[['CEGR[0,4]']] - l[['CEGR[-4,-1]']] ->
    l[['DID[1 year]']]


  ## ------------------------------------------------------------------------ ##
  ## WALD TEST                                                                ##
  ## ------------------------------------------------------------------------ ##
  l %>>%
    list.map({
      R <- t(.)

      waldtest(
        o,
        R = R,
        r = 0
      ) %>>%
        t %>>%
        data.table %>>%
        mutate(
          stat = .name,
          stat_value = (R %*% coef(o)) %>>% drop
        ) %>>%
        select(
          stat,stat_value, p, chi2, p.F, F, df1, df2
        )
    }) %>>%
  rbindlist -> output_tests

  output_tests %>>%
    mutate(
      star = ifelse(p < 0.01, "***",
                    ifelse(p < 0.05, "**",
                           ifelse(p < 0.1,"*",""))),
      print = sprintf("%s%s (p=%s)",
                      stat_value %>>% formatC(format = 'f',digits = 2),
                      star,
                      p %>>% formatC(format = 'f',digits = 2))
    ) ->
    output_tests

  return(output_tests)
}


testprep <- function(obj, tte_pattern = "factor\\(tte\\)",shift = 0){
  coef(obj) ->
    x

  data.table(
    coef.ix = 1:length(x),
    coef.name = names(x),
    tte = gsub(pattern = tte_pattern,replacement = "", names(x)) %>>% as.numeric
  ) %>>%
    subset(coef.name %like% tte_pattern) ->
    coef_df

  coef_df[, tte_shift := tte + shift]

  attr(coef_df,'obj') <- obj
  return(coef_df)
}
