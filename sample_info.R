##' @export
sample_info <- function(){
  imfutils::imfRefGroup() ->
    ref.group

  l <- list()

  ref.group %>>%
    subset(
      group %in% c(
        "Advanced Economies",
        "Organization for Economic Cooperation and Development (OECD)",
        "European Union",
        "Euro area (Euro Partners)"
      )
    ) ->
    l[['selected']]

  ref.group %>>%
    subset(
      group %in% c(
        "Advanced Economies"
      )
    ) %>>%
    mutate(
      group = 'AE'
    ) ->
    l[['AE']]

  ref.group %>>%
    subset(
      group %in% c(
        "Organization for Economic Cooperation and Development (OECD)"
      )
    ) %>>%
    mutate(
      group = 'OECD'
    ) ->
    l[['OECD']]

  ref.group %>>%
    subset(
            group %in% c(
        "European Union"
      )
    ) %>>%
    mutate(
      group = 'EU'
    ) ->
    l[['EU']]

  ref.group %>>%
    subset(
      group %in% c(
        "Euro area (Euro Partners)"
      )
    ) %>>%
    mutate(
      group = 'Eurozone'
    ) ->
    l[['Eurozone']]

  ref.group %>>%
    subset(
      group %in% "Emerging and developing economies"
    ) %>>%
    mutate(
      group = 'EME & Dev'
    ) ->
    l[['EME & Dev']]

  ref.group %>>%
    subset(
      group %in% "Emerging Market and Developing Economies: Low Income Developing Countries"
    ) %>>%
    mutate(
      group = 'LIC'
    )->
    l[['LIC']]

  l[['EME & Dev']] %>>%
    subset(!iso2 %in% l[['LIC']][['iso2']]) %>>%
    mutate(
      group = 'EME'
    )->
    l[['EME']]


  return(l)
}


