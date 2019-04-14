##' @export
execute_matching <- function(dt,formula,ratio){
  out <- list()

  dt %>>%
    copy %>>%
    na.omit %>>%
    data.table ->
    data

  matchit(
    formula = formula,
    data = data,
    model = 'logit',
    discard = 'both',
    ratio = ratio
    ## reestimate = TRUE
  ) ->
    out[['match_obj']]

  object = out[['match_obj']]
  weights = 'weight'
  distance = 'pscore'


  treat <- object[['treat']]
  wt <- object[['weights']]
  vars <- names(data)

  if (distance %in% vars)
    stop("invalid input for distance. choose a different name.")
  else if (!is.null(object$distance)) {
    data[, (distance) := object[['distance']]]
  }
  if (weights %in% vars)
    stop("invalid input for weights. choose a different name.")
  else if (!is.null(object$weights)) {
    data[, (weights) := object[['weights']]]
  }

  data ->
    out[['match_data']]


  return(out)
}
