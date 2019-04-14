##' @export
data_processor <- function(data){
  if (!"esdataset" %in% class(data)){
    stop('This function accepts only the datasets loaded via the "loader" function.')
  }

  data %>>%
    subset(
      year(date) >= 1997
    ) %>>%
    dplyr::select(
      iso3,date,
      incomeLevel,
      tte,
      event.date,
      event.id,
      event.n,
      event.ix,
      event.diff,
      event.name,
      banks,
      nonbanks,
      all,
      banks_PCHG,
      nonbanks_PCHG,
      all_PCHG,
      PCPIPCH,
      NGDP_PCHG,
      FILR,
      GFDD.OI.19,
      BCA_NGDPD,
      GGXCNL_NGDP,
      EQTY_INFLOW_NGDP,
      DEBT_INFLOW_NGDP,
      GFDD.DI.01_GDP_MULT,
      GFDD.DI.07_GDP_MULT,
      GFDD.DI.14_GDP_MULT,
      GFDD.DM.03_GDP_MULT,
      GFDD.DM.05_GDP_MULT,
      GFDD.DI.01_GDP_MULT_PCHG,
      GFDD.DI.07_GDP_MULT_PCHG,
      GFDD.DI.14_GDP_MULT_PCHG,
      GFDD.DM.03_GDP_MULT_PCHG,
      GFDD.DM.05_GDP_MULT_PCHG
    ) ->
    data_sel

  data_sel %>>%
    dplyr::mutate(
      region = ifelse(
        incomeLevel %in% c("High income: nonOECD",
                           "High income: OECD"),
        'AE',
        ifelse(
          incomeLevel %in% c('Low income',
                             'Lower middle income',
                             'Upper middle income'),
          'EME',
          "Unclassified"
        )
      )
    ) %>>%
    dplyr::mutate(
      banks_PCHG_Q = banks_PCHG / 4,
      nonbanks_PCHG_Q = nonbanks_PCHG / 4,
      all_PCHG_Q = all_PCHG /4,
      mktbk = iso3 %>>% classify_mkt.vs.bank,
      tte_q = tte * 4,
      .ix = sprintf('%s.%s.%s',
                    ifelse(is.na(event.date),'C','T'),
                    iso3,
                    event.ix)
    ) %>>% data.table ->
    data_sel2

  return(data_sel2)
}
