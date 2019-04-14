load_all()

require(data.table)
require(dplyr)
require(dtplyr)
require(pipeR)
require(rlist)
require(scales)
require(statar)

outname = "Figures"

dir.root = "/Users/Nander/Desktop/Done"
dir.output = "/Users/Nander/Desktop/Done"
dir.figs = "/Users/Nander/Desktop/Done"

country_classification() ->
  ref.ctry  #done?

ref.ctry[['All']] <- ref.ctry[['AE_EME.IMF']] %>>% mutate(region = 'All')

imfutils::imfRefCtry() %>>%
  select(iso3,name) %>>%
  setkey(iso3) ->
  ref.iso3

import_data <- function(){
  infile = file.path("[[PATH TO PACKAGE]]",
                     'inst/data',
                     sprintf('Event study data.RData',
                             vintage))

  load(infile)
}

import_data() ->
  l.events

ref.ctry[['AE_EME.IMF']] ->
  .classification

require(scales)
require(ggplot2)
library(extrafont)
loadfonts()
fonts()

l.events %>>% list.map({
  EVENTDB_NAME = .name
  EVENTDB = .
  EVENTDB[['l.events']] %>>% list.map({
    CLEAN_SCHEME_NAME = .name
    CLEAN_SCHEME = .
    CLEAN_SCHEME %>>%
      (dt ~ dt[,{
        list(N = .N)
      }, by = c('iso3','date','type')]) %>>%
      dcast(
        iso3 + date ~ type,
        value.var = 'N'
      ) %>>%
      mutate(
        Price = ifelse(is.na(Price),0,Price),
        Quantity = ifelse(is.na(Quantity),0,Quantity),
        All = Price + Quantity,
        FracQuant = 100 * Quantity / All
      ) %>>%
      subset(All > 0) ->
      events2

    (ref.iso3 %>>% setkey(iso3))[events2 %>>% setkey(iso3)] %>>%
      copy %>>%
      mutate(
        country = name %>>% factor(levels = rev(levels(name %>>% factor)))
      ) ->
      events2plot

    events2plot %>>%
      subset(!is.na(country)) %>>%
      (~ . %>>% (All) %>>% sum ->> N_ALL) %>>%
      ggplot(
        aes(
          x = date,
          y = country,
          group = country,
          size = All %>>% as.factor,
          colour = FracQuant
        )
      ) +
      geom_point(alpha = 0.8) +
      scale_size_manual(values=2*c(1,2,3,4)) +
      scale_colour_gradient(low = 'red',high = 'blue',limits = c(0,100)) +
      theme_bw() +
      labs(
        x = "Date",
        y = "",
        size = sprintf("# of MaPs\n(total = %s)",
                       N_ALL),
        colour = "Type:"
      ) ->
      p

    data.table(
      eventdb = EVENTDB_NAME,
      cleanscheme = CLEAN_SCHEME_NAME,
      plot = list(p)
    )
  }) %>>% rbindlist
}) %>>% rbindlist ->
  PLOTS


PLOTS %>>% apply(1,function(row){
  note = sprintf('%s | %s',
                 row[['eventdb']],  #e?
                 row[['cleanscheme']])

  message(note)

  figfile = file.path(dir.figs,sprintf("MaP Event Plot | %s | %s.png",
                                       row[['eventdb']],
                                       row[['cleanscheme']]))

  ggsave(plot = row[['plot']],
         filename = figfile,
         width = 6.5 * 2,
         height = 8.5 * 2,
         units = 'in')

  data.table(
    eventdb = row[['eventdb']],
    cleanscheme = row[['cleanscheme']],
    plot = list(row[['plot']]),
    figfile = figfile
  )
}) %>>% rbindlist ->
  PLOTS_PRODUCTION
