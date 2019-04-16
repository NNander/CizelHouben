##' @export
getSource <- function(x,parse = FALSE, split = '\\.'){
  FILE <- system.file('./data/Macro Data Lookup.xlsx',
                      package = 'MacroDatasets')

  openxlsx::read.xlsx(FILE,'Lookup') %>>%
    data.table ->
    data_lookup

  x = as.character(x)

  ## ref.labels %>>%
  ##   rbindlist ->
  ##   data_lookup

  setkey(data_lookup,'concept.id')

  if (parse == TRUE){
    tstrsplit(x,split = split) ->
      o
    if (length(o) == 1){
      o[[2L]] <- o[[1L]]
    }

    ifelse(is.na(o[[2L]]),o[[1L]],o[[2L]]) ->
      core

    ifelse(is.na(o[[2L]]),'not found',o[[1L]]) ->
      transf

    sprintf("%s (%s)",
            data_lookup[core][['concept_label']],
            transf) ->
      out
  } else {
    core = x
    data_lookup[core][['concept.source']] ->
      out
  }
  return(out)
}
