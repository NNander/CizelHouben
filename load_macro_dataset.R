##' @export
load_macro_dataset <- function(vintage = '2016-06-24'){
  FILE <- system.file(sprintf('./data/derived/%s yxdata.RData',vintage),
                      package = 'MacroDatasets')
  load(FILE)
  return(yxdata)
}



