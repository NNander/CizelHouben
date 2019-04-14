##' @export
batch_preparation_yxdata_BISsubset <- function(y.winsor.p = 0.025){
  prepare_ydata.oldnew() ->
    ydata

  ydata %>>%
    melt(
      id.vars = c('iso3','date')
    ) %>>%
    (dt ~ dt[, value_winsor := value %>>%
                 winsorize(
                   probs = c(y.winsor.p, 1- y.winsor.p),
                   verbose = FALSE
                 ), by = 'variable']) %>>%
    dcast(
      iso3 + date ~ variable,
      value.var = "value_winsor"
    ) ->
    ydata_winsor

  MacroDatasets::load_macro_dataset() %>>%
    dplyr::select(
      iso3,date,
      one_of(setdiff(names(.),names(ydata)))
    ) ->
      xdata

  (ydata_winsor %>>%
     setkey(iso3,date))[
       xdata %>>%
         setkey(iso3,date)
       ] %>>%
    melt(
      id.vars = c('iso3','date')
    ) %>>%
    dplyr::mutate(
      value = value %>>% .cleaner
    ) %>>%
    dcast(
      iso3 + date ~ variable,
      value.var = 'value'
    ) ->
    data_wide

  data_wide %>>%
    mutate(
      bankcrisis = ifelse(GFDD.OI.19>0,1,0) %>>% (x ~ ifelse(is.na(x),0,x))
    ) %>>%
    arrange(iso3,date) %>>%
    (dt~dt[,bankcrisis10y := {
      shift(bankcrisis,n = 0:40) %>>%
        data.frame %>>%
        rowSums(na.rm = TRUE) %>>%
        (. > 0) %>>%
        (. * 1)
    }, by = c('iso3')]) ->
    data_wide2

  data_wide2 %>>%
    subset(
      ! iso3 %in% c('ARG','GRC','SAU','MYS')
    ) %>>%
    subset(
      !is.na(pchgy_banks_lcu)
    ) ->
    data_wide3

  data_wide3 %>>%
    select(
      iso3,date,
      pchgy_banks_lcu,
      pchgy_nonbanks_lcu,
      GFDD.SI.01,
      NGDP_RPCH,
      FILR,
      bankcrisis,
      bankcrisis10y
    ) ->
    data_wide4

  list(
    ydata = ydata,
    ydata_winsor = ydata_winsor,
    xdata = xdata,
    data_wide = data_wide4
  ) ->
    out

  return(out)
}
