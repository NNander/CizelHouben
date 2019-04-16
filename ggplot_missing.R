##' @export
ggplot_missing <- function(data, save = TRUE, width = 10, height = 10){
  data %>>%
    list.map({
      x <- .

      n.miss = is.na(x) %>>% sum
      n = x %>>% length
      missrate = n.miss/n

      data.table(
        name = .name,
        n = n,
        n.miss = n.miss,
        missrate = missrate
      )
    }) %>>%
  rbindlist %>>% arrange(missrate) -> dt

  data %>>%
    is.na %>>%
    melt %>>%
    data.frame %>>%
    mutate(
      Var2 = factor(Var2,levels = dt[['name']])
    ) %>>%
    ggplot(
      aes(
        x = Var2,
        y = Var1
      )
    ) +
    geom_raster(aes(fill = value)) +
    scale_fill_grey(name = "",
                    labels = c("Present","Missing")) +
    theme_bw() +
    theme(axis.text.x  = element_text(angle=90,hjust = 1)) +
    labs(x = "Variables in Dataset",
         y = "Rows / observations") ->
    p

    p 
  return(dt)
}
