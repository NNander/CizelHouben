##' @export
report_lrm <- function(obj,digits = 3){            #obj is a list of rlm objects
  o <- obj
  o %>>%
    list.map({
      list(
        . %>>% extract_estimates(digits = digits) %>>%
          setnames(old = '.value',new = .name)
      ) %>>%
        rbindlist
    }) %>>%
    Reduce(f = function(...) merge(...,by = '.ix', all.x = TRUE, all.y = TRUE)) %>>%
    (dt~dt[,c('var','order'):=tstrsplit(.ix,split = ":")]) %>>%
    (dt~dt[,c('var2','flevel'):=tstrsplit(var,split = "=")]) %>>%
    mutate(
      label = var2 %>>% getLabel %>>% gsub(pattern = "\n",replacement = " ")
    ) %>>%
    mutate(
      label = ifelse(is.na(label),
                     sprintf("%s (=%s)",var,flevel),
                     label)
    ) %>>%
    mutate(
      label_extended = sprintf("%s [%s]",label,var)
    ) %>>%
    (dt~dt[order==2,label := ""]) %>>%
    (dt~dt[order==2,label_extended := ""]) ->
    out_coef

  o %>>%
    list.map({
      list(
        . %>>% extract_stats.lrm %>>%
          setnames(old = '.value',new = .name)
      ) %>>%
        rbindlist
    }) %>>%
    Reduce(f = function(...) merge(...,by = '.ix', all.x = TRUE, all.y = TRUE)) ->
    out_stat

  o %>>%
    list.map({
      coef(.) %>>% names
    }) %>>%
    Reduce(f = c) %>>%
    unique ->
    var_order

  out_coef %>>% (var) %>>% unique

  out_coef %>>%
    mutate(var = var %>>% factor(levels = var_order)) %>>%
    arrange(var,order) ->
    out_coef


  list(
    out_coef %>>% select(label = label_extended,one_of(names(o))),
    out_stat %>>% select(label = .ix, one_of(names(o)))
  ) %>>%
    rbindlist ->
    out

  return(out)
}


