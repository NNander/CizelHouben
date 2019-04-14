##' @export
report_felm <- function(o, digits = 3){
  o %>>%
    list.map({
      obj_name = .name
      obj = .

      obj %>>%
        extract_estimates(digits = digits) %>>%
        setnames(old = '.value',new = obj_name)
    }) %>>%
  Reduce(f = function(...) merge(...,by = '.ix', all.x = TRUE, all.y = TRUE)) %>>%
  mutate(
    label_pub = .ix,
    var = gsub("(.+):(.)","\\1",.ix),
    order = gsub("(.+):(.)","\\2",.ix)
  ) %>>%
  (dt~dt[grepl(":2",.ix),label_pub := ""]) ->
  out_coef

  o %>>%
    list.map({
      obj_name = .name
      obj = .
      obj %>>%
        extract_stats.felm %>>%
        setnames(old = '.value',new = obj_name)
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

  out_coef %>>%
    mutate(var = var %>>% factor(levels = var_order)) %>>%
    arrange(var,order) ->
    out_coef

  list(
    out_coef %>>% select(label = .ix, one_of(names(o))),
    out_stat %>>% select(label = .ix, one_of(names(o)))
  ) %>>%
    rbindlist ->
    out

  return(out)
}

