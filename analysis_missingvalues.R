##' @export
analysis_missingvalues <- function(data, label = ''){
  data %>>%
    summary_missing %>>%
    dplyr::mutate(
      label = variable %>>% getLabel,
      source = variable %>>% getSource
    ) %>>%
    dplyr::select(
      variable,label,source,n_cs,year_min,year_max,year_mean,n,n.miss,missrate
    ) ->
    outputs[['Missingness']]

  data %>>%
    summary_allmissing(xcolIsIdcol = FALSE) %>>%
    dplyr::mutate(
      label = id %>>% getLabel,
      source = id %>>% getSource
    ) ->
    outputs[['Missingness Pattern']]

  data %>>%
    summary_allmissing(xcolIsIdcol = FALSE) %>>%
    attr('nmiss.byid') ->
    outputs[['All missing by country']]

  data %>>%
    summary_allmissing(xcolIsIdcol = FALSE) %>>%
    attr('nmiss.byvar') %>>%
    dplyr::mutate(
      label = variable %>>% getLabel,
      source = variable %>>% getSource
    ) %>>%
    dplyr::select(
      variable,label,source,nmiss.byvar
    ) ->
    outputs[['All missing by variable']]

  return(outputs)
}
