##' @export
prepare_macro_labels <- function(outfile = NULL){

  ## ------------------------------------------------------------------------ ##
  ## IMF REFERENCE                                                            ##
  ## ------------------------------------------------------------------------ ##
  r <- list()
  l <- list()

  imfutils::imfRefCtry() %>>%
    dplyr::mutate(imfctry = as.numeric(imfctry)) ->
    ref

  imfutils::imfRefGroup() ->
    ref.group

  r[['Country Reference']] <- ref
  r[['Country Groups']] <- ref.group


  ## ## ------------------------------------------------------------------------ ##
  ## ## OECD                                                                     ##
  ## ## ------------------------------------------------------------------------ ##
  ## oecdutils::prepare_OECD_credit_data() ->
  ##   oecd_credit

  ## oecd_credit %>>%
  ##   dplyr::select(sector_id,sector_label) %>>%
  ##   unique ->
  ##   l[['OECD Credit -- Sectors']]

  ## oecd_credit %>>%
  ##   attr('sourcedata') %>>%
  ##   attr('lookup') ->
  ##   l[['OECD Credit -- Balance Sheets']]


  ## ------------------------------------------------------------------------ ##
  ## IMF WEO                                                                  ##
  ## ------------------------------------------------------------------------ ##
  imfutils::loadWEO2015oct() %>>%
    attr('lookup') %>>%
    dplyr::select(
      concept.id = concept_id,
      concept.label = concept_label
    ) ->
    l[['IMF-WEO']]

  ## ------------------------------------------------------------------------ ##
  ## IMF FSI                                                                  ##
  ## ------------------------------------------------------------------------ ##
  imfutils::loadFSI2016() %>>%
    attr('lookup') %>>%
    dplyr::select(
      concept.id = concept_id,
      concept.label = concept_label
    ) ->
    l[['IMF-FSI']]

  ## ------------------------------------------------------------------------ ##
  ## IMF IFS                                                                  ##
  ## ------------------------------------------------------------------------ ##
  imfutils::loadIFS2016() %>>%
    attr('lookup') %>>%
    dplyr::select(
      concept.id = concept_id,
      concept.label = concept_label
    ) ->
    l[['IMF-IFS']]

  ## ------------------------------------------------------------------------ ##
  ## WB GFDD                                                                  ##
  ## ------------------------------------------------------------------------ ##
  wbutils::process_wbgfdd() %>>%
    attr('lookup') %>>%
    dplyr::select(
      concept.id,concept.label
    )->
    l[['World Bank GFDD']]

  ## ------------------------------------------------------------------------ ##
  ## BIS DATA                                                                 ##
  ## ------------------------------------------------------------------------ ##
  bisutils::loadBISlabels() ->
    l[['BIS']]

  ## ------------------------------------------------------------------------ ##
  ## AMECO                                                                    ##
  ## ------------------------------------------------------------------------ ##
  amecoutils::loadAMECOselected() %>>%
    attr('lookup') %>>%
    setnames(
      old = c('id','name'),
      new = c('concept.id','concept.label')
    )->
    l[['AMECO']]

  l %>>%
    list.map({
      . %>>% dplyr::mutate(concept.source = .name)
    }) %>>%
    rbindlist ->
    r[['Lookup']]

  if (is.null(outfile)){
    outfile = file.path(dir.root,sprintf('inst/data/%s Macro Data Lookup.xlsx',Sys.Date()))
  }

  createWorkbook() ->
    wb

  r %>>%
    list.map({
      addWorksheet(wb,.name)
      writeData(wb,.name,.)
    })

  saveWorkbook(wb, outfile,
               overwrite = TRUE)

  return(invisible(r))
}
