##' @export
loadYXdataset <- function(vintage = '2016-07-29'){
  FILE <- system.file(sprintf('./data/derived/%s yxdata.RData',vintage),
                      package = 'MacroDatasets')
  load(FILE)
  return(yxdata)
}

## vintage = '2016-06-18'
