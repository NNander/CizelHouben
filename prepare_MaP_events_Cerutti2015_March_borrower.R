##' @export
prepare_MaP_events_Cerutti2015_March_borrower <- function(){
  FILE <- c('[PATH TO FILE]/cerutti.RData')
  load(FILE)

  cerutti %>>%
    dplyr::select(
      iso3, date,
      MPI = `MPI (Table A1_Annex1)`,
      LTV,LTV_CAP,
      DTI,DP,CTC,LEV,SIFI,INTER,CONC,FC,RR,RR_REV,CG,TAX
    ) %>>%
    mutate(
      `Quantity` = LTV + DTI + LEV + INTER + CONC + FC + RR + CG,
      `Price` = DP + CTC + SIFI + TAX,
      borrower = LTV_CAP + DTI,
      institution = DP + CTC + LEV + SIFI + INTER + CONC + FC + RR_REV + CG + TAX,
      All = Quantity + Price
    ) %>>%
    data.table %>>%
    melt(
      id.vars = c('iso3','date')
    ) %>>%
    mutate(
      value = as.numeric(value)
    ) %>>%
    data.table %>>%
    split(.[['variable']]) %>>%
    list.map({
      policy = .name
      data = copy(.)

      data %>>%
        ## WAS NOT SORTED!!
        arrange(variable, iso3) %>>%
        data.table %>>%
        (dt~dt[,chg.value := value - shift(value,1), by = c('variable','iso3')]) %>>%
        subset(!is.na(chg.value)) %>>%
        subset(chg.value != 0)
    }) ->
  l.events


  return(l.events)
}

