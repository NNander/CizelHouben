load_all()

require(data.table)
require(dplyr)
require(dtplyr)
require(pipeR)
require(rlist)

options(dir.root = "PATH TO ROOT")

import_data <- function(){
  infile = file.path("[[PATH TO PACKAGE]]",
                     'inst/data',
                     sprintf('Event study data.RData',
                             vintage))

  load(infile)
}

import_data() ->
  l.events

#does not work 
country_classification() ->
  ref.ctry

ref.ctry[['All']] <- ref.ctry[['AE_EME.IMF']] %>>% mutate(region = 'All')

#does not work, for same reason as country_classification
batch_preparation_yxdata() ->
  l.yxdata

l.yxdata[['data_wide']] %>>%
  copy %>>%
  arrange(iso3,date) %>>%
  (dt~dt[,pchgy_banks_lcu.lag := shift(pchgy_banks_lcu, n = 4, type = 'lag'),
         by = list(iso3)]) ->
  l.yxdata[['data_wide']]


.cleaner <- function(x){
  x %>>%
    (ifelse(is.infinite(.),NA,.)) %>>%
    (ifelse(is.nan(.),NA,.))
}

l.events %>>% list.map({
  EVENTDB_NAME = .name
  l.event_data = .
  l.event_data[['event_db']][['event_panel']] ->
    data

  data %>>%
    copy %>>%
    arrange(
      iso3,date
    ) %>>%
    (dt ~ dt[, MaPin2years := {
      shift(.dum_event,n = 0:8,type = 'lead') %>>%
        data.frame %>>%
        rowSums(na.rm = TRUE) %>>%
        (. > 0) %>>%
        (. * 1) ->
        o.pre

      shift(.dum_event,n = 1:4,type = 'lag') %>>%
        data.frame %>>%
        rowSums(na.rm = TRUE) %>>%
        (. > 0) %>>%
        (. * 1) ->
        o.post

      o.pre[o.post == 1] <- NA
      o.pre
    }, by = 'iso3']) -> data2

  (l.yxdata[['data_wide']] %>>%
     setkey(iso3,date))[
       data2 %>>%
         setkey(iso3,date)
       ] %>>%
    subset(
      ! iso3 %in% c('ARG','GRC','SAU','MYS')
    ) %>>%
    (dt~dt[,bankcrisis10y := {
      shift(bankcrisis,n = 0:40) %>>%
        data.frame %>>%
        rowSums(na.rm = TRUE) %>>%
        (. > 0) %>>%
        (. * 1)
    }, by = c('iso3')]) %>>%
    mutate(
      TFE = date %>>% factor,
      CFE = iso3 %>>% factor
    ) ->
    esdata

  data.table(
    eventdb = EVENTDB_NAME,
    eventdb_obj = list(l.event_data),
    esdata = list(esdata)
  )
}) %>>% rbindlist -> l.esdata

l.model <- list()

l.model[['Bench']] <-
  c('MaPin2years',
    'pchgy_banks_lcu',
    'pchgy_nonbanks_lcu0D',
    'TFE'
    )

l.model[['Bench + CB']] <- c(
  l.model[['Bench']],
  'pchgy_cb2all'
)


l.model[['Bench + Z']] <-  c(
  l.model[['Bench']],
  'pchgy_cb2all',
  'GFDD.SI.01'
)

l.model[['Bench + Z + Crisis']] <-  c(
  l.model[['Bench']],
  'pchgy_cb2all',
  'GFDD.SI.01',
  'bankcrisis10y'
)

require(rms)
require(corrplot)
library(multcomp)
library(metap)

## EVENT DATABASE
l.esdata %>>% apply(1,function(row){
  EVENTDB_NAME = row[['eventdb']]
  EVENTDB = row[['eventdb_obj']]
  dataset = row[['esdata']]
  ## COUNTRY CLASSIFICATION
  ref.ctry %>>% list.map({
    CLASSIFICATION_NAME = .name
    CLASSIFICATION = .
    CLASSIFICATION %>>%
      copy %>>%
      setnames(
        old = names(.),
        new = c('iso3','class')
      ) %>>%
      split(.[['class']]) %>>% list.map({
        CLASS_NAME = .name
        CLASS = .
        l.model %>>% list.map({
          MODEL_NAME = .name
          MODEL = .
          note = sprintf("Matching - Stage 1 | %s | %s | %s | %s",
                         EVENTDB_NAME,
                         CLASSIFICATION_NAME,
                         CLASS_NAME,
                         MODEL_NAME
                         )
          message(note)

          FORMULA = char2formula(MODEL)

          dataset %>>%
            subset(iso3 %in% CLASS[['iso3']]) %>>%
            subset(year(date)>=1996) %>>%
            dplyr::select(one_of(MODEL)) ->
            data

          data %>>%
            na.omit %>>%
            lrm(formula = FORMULA) ->
            obj

          data.table(
            caseid = note,
            eventdb = EVENTDB_NAME,
            classification = CLASSIFICATION_NAME,
            class = CLASS_NAME,
            model = MODEL_NAME,
            model_vars = list(MODEL),
            formula = list(FORMULA),
            data = list(data),
            estobj = list(obj)
          )
        }) %>>% rbindlist
      }) %>>% rbindlist
  }) %>>% rbindlist
}) %>>% rbindlist -> RESULTS
