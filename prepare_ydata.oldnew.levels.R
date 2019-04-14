##' @export
prepare_ydata.oldnew.levels <- function(){

  l.depvars.old <- list(
    banks = 'banks',
    nonbanks = 'nonbanks',
    all = 'all',
    wb_bankcredit = 'GFDD.DI.01_GDP_MULT',
    wb_ivassets = 'GFDD.DI.07_GDP_MULT',
    wb_allcredit = 'GFDD.DI.14_GDP_MULT',
    wb_debtissue_dom = 'GFDD.DM.03_GDP_MULT',
    wb_debtissue_int = 'GFDD.DM.05_GDP_MULT'
  )


  c('all','quantity','price') %>>%
    list.map({
      loader(.) %>>%
        data_processor %>>%
        dplyr::select(
          iso3,date,
          one_of(
            as.character(unlist(l.depvars.old))
          )) %>>%
        setnames(
          old = as.character(unlist(l.depvars.old)),
          new = names(l.depvars.old)
        ) %>>%
        melt(
          id.vars = c('iso3','date')
        ) %>>%
        subset(!is.na(value)) %>>%
        subset(value != 0) %>>%
        setkey(iso3,date,variable) %>>%
        unique
    }) %>>%
    rbindlist %>>%
    setkey(iso3,variable,date) %>>%
    unique %>>%
    mutate(
      variable = sprintf("%s_old",variable)
    )->
    ydata.old.long

  .cleaner <- function(x){
    x %>>%
      (ifelse(is.infinite(.),NA,.)) %>>%
      (ifelse(is.nan(.),NA,.))
  }

  MacroDatasets::load_macro_dataset() %>>%
    subset(
      year(date) >= 1995
    ) ->
    yxdata

  yxdata %>>%
    dplyr::mutate(
      GFDD.DI.01_L = GFDD.DI.01 * NGDP,
      GFDD.DI.07_L = GFDD.DI.07 * NGDP,
      GFDD.DI.11_L = GFDD.DI.11 * NGDP,
      GFDD.DI.14_L = GFDD.DI.14 * NGDP,
      GFDD.DM.03_L = GFDD.DM.03 * NGDP,
      GFDD.DM.05_L = GFDD.DM.05 * NGDP
    ) %>>%
    dplyr::select(
      iso3,date,
      matches('^banks'),
      matches('^nonbanks'),
      matches('^all'),
      matches('^credit'),
      matches('^cb2'),
      wb_bankcredit = GFDD.DI.01_L,
      wb_ivassets = GFDD.DI.07_L,
      wb_insuranceassets = GFDD.DI.11_L,
      wb_allcredit = GFDD.DI.14_L,
      wb_debtissue_dom = GFDD.DM.03_L,
      wb_debtissue_int = GFDD.DM.05_L
    ) %>>%
    melt(
      id.vars = c('iso3','date')
    ) %>>%
    dcast(
      iso3 + date ~ variable, value.var = c('value')
    ) ->
    ydata.new

  list(
    ydata.old.long,
    ydata.new %>>% melt(id.vars = c('iso3','date'))
  ) %>>% rbindlist %>>%
    subset(!is.na(value)) %>>%
    dcast(
      iso3 + date ~ variable,
      value.var = 'value'
    ) ->
    ydata

  return(ydata)
}

