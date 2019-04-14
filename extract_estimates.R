##' Extract estimates from estimation object
##'
##' @export
extract_estimates <- function(o,digits = 3,
                              coef.extractor = coef,
                              vcov.extractor = vcov){
  list(
    coef = coef.extractor(o) %>>%
      data.frame %>>%
      dplyr::rename(.value = .) %>>%
      dplyr::mutate(.ix = rownames(.)) %>>%
      dplyr::select(.ix,.value) %>>%
      data.table %>>%
      setnames(
        old = '.value',
        new = 'coef'
      ),
    se = vcov(o) %>>%
      diag %>>%
      data.frame %>>%
      dplyr::rename(.value = .) %>>%
      dplyr::mutate(.ix = rownames(.)) %>>%
      dplyr::mutate(.value = sqrt(.value)) %>>%
      dplyr::select(.ix,.value) %>>%
      data.table %>>%
      setnames(
        old = '.value',
        new = 'se'
      )
  ) ->
    l

  (l[['coef']] %>>% setkey(.ix))[
    l[['se']] %>>% setkey(.ix)
    ] %>>%
    mutate(
      zstat = coef/se,
      pval = 1.96 * pt(-abs(zstat), df = Inf),
      stars = ifelse(pval > 0.1,"",
                     ifelse(pval > 0.05, "*",
                            ifelse(pval > 0.01, '**','***')))
    ) ->
    l[['test']]

  list(
    l[['test']] %>>%
      mutate(
        .ix = sprintf('%s:1',.ix),
        .value = sprintf("%s%s",
                         coef %>>% formatC(format = 'f', digits = digits),
                         stars)
      ) %>>%
      dplyr::select(
        .ix,.value
      ),
    l[['test']] %>>%
      mutate(
        .ix = sprintf('%s:2',.ix),
        .value = sprintf(
          "(%s)",
          se %>>% formatC(format = 'f', digits = digits)
        )
      ) %>>%
      dplyr::select(.ix,.value)
  ) %>>%
    do.call(what = 'interleave') ->
    out

  attr(out,'raw_estimates') <- l[['test']]
  return(out)
}
