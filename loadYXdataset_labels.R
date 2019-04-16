##' @export
loadYXdataset_labels <- function(){
  FILE.XL <- system.file('./data/Macro Data Lookup.xlsx',
                         package = 'MacroDatasets')

  openxlsx::read.xlsx(xlsxFile = FILE.XL,sheet = 'Lookup') ->
    lookup

  return(lookup)
}


