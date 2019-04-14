##'
##' Exclusion policies:
##'   consevative: exclude all events preceded by another within
##'   windon_months
##'
##'   lax: only exclude events of the SAME type
##' @export
exclude_overlapping_events_v2 <- function(obj, # obj returned by events2dataset
                                          window_months = 12,
                                          idcol = NULL,
                                          typecol = NULL,
                                          exclusion = c('conservative','lax')){

  if (! "events2dataset"  %in% class(obj)){
    error('`obj` must be the one returned by `events2dataset` function')
  }


  if (exclusion == 'conservative'){
    obj %>>%
      copy %>>%
      (treatment_lookup) %>>%
      arrange_(idcol,'mid') %>>%
      (df~df[, months_since_last := {
        diff_months(shift(mid,1),mid)
      }, by = idcol]) ->
      treatment_lookup

    treatment_lookup[,keep := months_since_last >= window_months]
    treatment_lookup[is.na(keep),keep := TRUE]
  } else if (exclusion == 'lax'){
    if (is.null(typecol))
      stop('If exclusion policy is lax, you must specify typecol containing the event type.')

    obj %>>%
      copy %>>%
      (treatment_lookup) %>>%
      arrange_(idcol,typecol,'mid') %>>%
      (df~df[, months_since_last := {
        diff_months(shift(mid,1),mid)
      }, by = c(idcol,typecol)]) ->
      treatment_lookup

    treatment_lookup[,keep := months_since_last >= window_months]
    treatment_lookup[is.na(keep),keep := TRUE]
  } else {
    stop('Specify valid exclusion policy.')
  }

  treatment_lookup %>>%
    subset(keep == TRUE) ->
    treatment_lookup

  ## treatment_lookup[iso3 == 'ARG'] %>>% select(iso3,date,months_since_last,type)

  ## Subset step


  obj %>>%
    (treatment) %>>%
    subset(
      .ix %in% treatment_lookup[['.ix']]
    ) ->
    treatment

  obj[['treatment']] <- treatment
  obj[['treatment_lookup']] <- treatment_lookup

  attr(obj, 'class') <- c(class(obj),'exclude_overlapping_events')

  return(obj)
}
