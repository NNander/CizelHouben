##' @import data.table pipeR rlist openxlsx ggplot2 lfe eventstudy ggthemes
##' @importFrom dplyr mutate select arrange
##' @importFrom stringr str_trim
##' @importFrom whisker whisker.render
##' @importFrom statar winsorize
##' @importFrom gdata interleave
##' @importFrom Hmisc escapeRegex
NULL

.onLoad <- function(libname = find.package('PACKAGE'),
                    pkgname = 'PACKAGE'){

  DIR <- list(
    DIR.ROOT = "[FILE PATH]",
    DIR.OUTPUT.ROOT = "[FILE PATH]"
  )

  DIR.OUTPUT = file.path(DIR[['DIR.OUTPUT.ROOT']],Sys.Date())
  dir.create(DIR.OUTPUT)

  DIR = c(DIR, DIR.OUTPUT = DIR.OUTPUT)

  DIR %>>% list.map({
    ALIAS = .name
    PATH = .
    assign(ALIAS,PATH,
           envir = .GlobalEnv)
  })

  options(width = 80)
}

