##' @export
save_stata <- function(
                       data = NULL,
                       lookupinfo = NULL,
                       file = '~/Downloads/testfile.csv'
                       ){

  sanitizer <- function(str){
    str %>>%
      gsub(pattern = "[[:punct:]|[:space:]]", replacement = "_") %>>%
      gsub(pattern = "_$", replacement = "_E") %>>%
      gsub(pattern = "^_$", replacement = "V_") %>>%
      gsub(pattern = "^([[:digit:]].+)", replacement = "V\\1")
  }


  data %>>%
    copy %>>%
    setnames(
      old = data %>>% names,
      new = data %>>% names %>>% sanitizer
    ) ->
    dt

  write.csv(
    dt,
    file = file,
    row.names = FALSE,
    na = ''
  )

  file %>>%
    gsub(pattern = '(.+)/(.+)\\.csv',
         replacement = "\\1/\\2-labels.do") ->
    con

  sprintf(
    'label variable %s "%s"',
    lookupinfo[['NAME']] %>>% sanitizer,
    lookupinfo[['LABEL']]
  ) %>>%
    writeLines(con = con)

  return(NULL)
}
