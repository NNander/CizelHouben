##' @export
country_classification <- function(){
  ## ------------------------------------------------------------------------ ##
  ## OLD (WB-BASED) AE/EME CLASSIFICATION                                     ##
  ## ------------------------------------------------------------------------ ##
  c('all','quantity','price') %>>%
  list.map({
    loader(.) %>>%
      data_processor
  }) ->
    l.data

  l.data %>>%
    list.map({
      . %>>%
        data.table %>>%
        dplyr::select(
          iso3,incomeLevel,region
        ) %>>%
        setkey(iso3) %>>%
        unique
    }) %>>%
    rbindlist %>>%
    setkey(iso3) %>>%
    unique %>>%
    dplyr::select(iso3,region) ->
    ctryclass.wb

  ## ------------------------------------------------------------------------ ##
  ## NEW (IMF-BASED) AE/EME CLASSIFICATION                                    ##
  ## ------------------------------------------------------------------------ ##
  Projects2016.FiscalStress::sample_info()[c('AE','EME')] %>>%
    rbindlist %>>%
    dplyr::select(iso3,region = group) ->
    ctryclass.imf

  ## ------------------------------------------------------------------------ ##
  ## MARKET VS BANK-BASED CLASSIFICATIONS                                     ##
  ## ------------------------------------------------------------------------ ##
  imfutils::imfRefCtry() %>>%
    dplyr::mutate(
      mktbkclass = iso3 %>>% classify_mkt.vs.bank
    ) %>>%
    select(
      iso3, mktbkclass
    ) ->
    ctryclass.mktbk


  out = list(
    AE_EME.IMF = ctryclass.imf,
    AE_EME.WB = ctryclass.wb,
    MKT_BK = ctryclass.mktbk
  )

  return(out)
}
