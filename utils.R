##' @export 
char2formula <- function(x){
    sprintf(
        "%s ~ %s",
        x[1L],
        x[-1L] %>>% paste(collapse = " + ")
    ) %>>%
        as.formula ->
        o

    return(o)
}

