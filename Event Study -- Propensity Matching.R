load_all()

require(data.table)
require(dplyr)
require(dtplyr)
require(pipeR)
require(rlist)
require(scales)
require(statar)

outname = "Matching-based CEGR"

dir.output = file.path(options('dir.output'),Sys.Date())
dir.create(dir.output)
dir.figs = file.path(dir.output,'figures')
dir.create(dir.figs)

country_classification() ->
  ref.ctry

ref.ctry[['All']] <- ref.ctry[['AE_EME.IMF']] %>>% mutate(region = 'All')

batch_preparation_yxdata() ->
  l.yxdata

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

## Models
l.model <- list()

l.model[['Bench']] <-
  c('treated',
    'pchgy_banks_lcu',
    'pchgy_nonbanks_lcu'
    )

l.model[['Bench + CB']] <-
  c('treated',
    'pchgy_banks_lcu',
    'pchgy_nonbanks_lcu',
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

options(dir.root = "PATH TO ROOT")

l.events %>>% list.map({
  EVENTDB_NAME = .name
  l.event_data = .
  l.event_data[['event_db']][c('treatment','control')] %>>%
    list.map({
      . %>>%
        mutate(
          group = .name
        )
    }) %>>%
    rbindlist ->
    data

  vars.logit <- l.model %>>% unlist %>>% unique %>>%
    intersect(names(l.yxdata[['data_wide']]))

  (l.yxdata[['data_wide']] %>>%
     copy %>>%
     setkey(iso3,date))[
       data %>>%
         setkey(iso3,date)
       ] %>>%
    dplyr::select(
      .ix,tte,iso3,date,
      one_of(vars.logit)
    ) %>>%
    subset(
      tte %between% c(-2,0)
    ) %>>%
    melt(
      id.vars = c('iso3','date','.ix','tte')
    ) %>>%
    subset(
      !is.na(value)
    ) %>>%
    (dt ~ dt[,{
      list(
        iso3 = iso3 %>>% unique,
        value = value %>>%
          mean(na.rm = TRUE) %>>%
          .cleaner
      )
    }, by = c('.ix','variable')]) %>>%
    dcast(
      .ix  ~ variable,
      value.var = 'value'
    ) ->
    dataset_aggregated

  (l.event_data[['event_db']][c('treatment_lookup','control_lookup')] %>>%
     rbindlist %>>%
     dplyr::select(
       .ix, date
     ) %>>%
     setkey(.ix))[
       dataset_aggregated %>>%
         setkey(.ix)
       ] ->
    dataset_aggregated2

  dataset_aggregated2 %>>%
    mutate(
      iso3 = substr(.ix,3,5),
      eyear = year(date),
      bankcrisis10y = ifelse(bankcrisis10y>0,1,0) %>>% (x ~ ifelse(is.na(x),0,x)),
      treated = ifelse(.ix %like% "^T",1,0),
      TFE = date %>>% year %>>% factor
    ) %>>%
    subset(
      ! iso3 %in% c('ARG','GRC','SAU','MYS')
    ) ->
    dataset_aggregated3

  ## Stage 2 data
  (l.yxdata[['data_wide']] %>>%
     setkey(iso3,date))[
       l.event_data[['event_db']][['event_panel_expanded']] %>>%
         setkey(iso3,date)
       ] %>>%
    subset(
      ! iso3 %in% c('ARG','GRC','SAU','MYS')
    ) %>>%
    mutate(
      type = ifelse(is.na(type), 'control',as.character(type))
    ) ->
    esdata

  data.table(
    eventdb = EVENTDB_NAME,
    eventdb_obj = list(l.event_data),
    data_logit = list(dataset_aggregated3),
    esdata = list(esdata)
  )
}) %>>% rbindlist -> l.esdata

## -------------------------------------------------------------------------- ##
## STAGE 1: LOGISTIC REGRESSION                                               ##
## -------------------------------------------------------------------------- ##
require(rms)
require(corrplot)
require(MatchIt)

width = 5
height = 5
units = 'in'
par(mar = c(2,2,2,2))

l.esdata %>>% apply(1,function(row){
  EVENTDB_NAME = row[['eventdb']]
  EVENTDB = row[['eventdb_obj']]
  dataset = row[['data_logit']]
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
            dplyr::select(.ix,one_of(MODEL)) ->
            data

          try({
            data %>>%
              na.omit %>>%
              execute_matching(formula = FORMULA, ratio = 10)
          }) ->
            obj

          if ('try-error' %in% class(obj)){
            data.table(
              caseid = note,
              eventdb = EVENTDB_NAME,
              classification = CLASSIFICATION_NAME,
              class = CLASS_NAME,
              class_countries = list(CLASS),
              model = MODEL_NAME,
              model_vars = list(MODEL),
              formula = list(FORMULA),
              eventdb_obj = list(row[['eventdb_obj']]),
              data = list(data),
              data_logit = list(row[['data_logit']]),
              esdata = list(row[['esdata']]),
              estobj = NA,
              figfile.jitter = NA,
              figfile.hist = NA
            )
          } else {
            par(mfrow = c(1,1))
            plot(obj[['match_obj']], type = 'jitter',interactive = FALSE)
            recordPlot() -> p1

            plot(obj[['match_obj']], type = 'hist',interactive = FALSE)
            recordPlot() -> p2

            figfile.jitter = file.path(dir.figs,sprintf("%s | Jitter.png",
                                                        note))
            png(file = figfile.jitter,
                width = width,
                height = height,
                units = units,
                res = 200)
            print(p1)
            dev.off()

            figfile.hist = file.path(dir.figs,sprintf("%s | Histogram.png",
                                                      note))
            png(file = figfile.hist,
                width = width,
                height = height,
                units = units,
                res = 200)
            print(p2)
            dev.off()

            row[['esdata']] %>>%
              subset(iso3 %in% CLASS[['iso3']]) ->
              esdata

            ## Add match data
            (obj[['match_data']] %>>%
               dplyr::select(
                 .ix,pscore,weight
               ) %>>%
               setkey(.ix))[
                 esdata %>>%
                   setkey(.ix)
                 ] %>>%
              subset(!is.na(weight)) %>>%
              subset(weight > 0) ->
              esdata2

            esdata2 %>>%
              mutate(
                group = ifelse(grepl("^T",.ix),'treatment','control'),
                tte_q = ifelse(group == 'control',
                               -999,
                               tte)
              ) %>>%
              dplyr::select(
                .ix,pscore,weight,iso3,date,tte_q,
                type,group,
                matches('pchgy')
              ) ->
              esdata3


            data.table(
              caseid = note,
              eventdb = EVENTDB_NAME,
              classification = CLASSIFICATION_NAME,
              class = CLASS_NAME,
              class_countries = list(CLASS),
              model = MODEL_NAME,
              model_vars = list(MODEL),
              formula = list(FORMULA),
              eventdb_obj = list(row[['eventdb_obj']]),
              data = list(data),
              data_logit = list(row[['data_logit']]),
              esdata = list(esdata3),
              estobj = list(obj),
              figfile.jitter = figfile.jitter,
              figfile.hist = figfile.hist
            )
          }
        }) %>>% rbindlist
      }) %>>% rbindlist
  }) %>>% rbindlist
}) %>>% rbindlist -> RESULTS_STAGE1


## -------------------------------------------------------------------------- ##
## STAGE 2                                                                    ##
## -------------------------------------------------------------------------- ##
l.depvars <- list(
  `Banks_LCU` = "pchgy_banks_lcu",
  `Nonbanks_LCU` = "pchgy_nonbanks_lcu",
  `All_LCU` = "pchgy_all_lcu",
  `Banks_USD` = "pchgy_banks_usd",
  `Nonbanks_USD` = "pchgy_nonbanks_usd",
  `All_USD` = "pchgy_all_usd"
)

l.class_quantprice <- list(
  'All' = 'All',
  'Quantity' = 'Quantity',
  'Price' = 'Price'
)

window = c(-12,9)
shift = 4
tte_var = 'factor(tte_q)'
fe = 'factor(date)'

RESULTS_STAGE1 %>>%
  subset(!is.na(estobj)) %>>%
  apply(1,function(row){
    EVENTDB = row[['eventdb_obj']]
    esdata = row[['esdata']]
    EVENTDB[['Clean overlapping events']] %>>% list.map({
      CLEAN_SCHEME_NAME = .name
      CLEAN_SCHEME = . %>>% (.ix) %>>% unique
      l.class_quantprice %>>% list.map({
        TOOL_NAME = .name
        TOOL = .
        l.depvars %>>% list.map({
          DEPVAR_NAME = .name
          DEPVAR = .

          note = sprintf("CEGR | %s | %s | %s | %s | %s | %s | %s",
                         row[['eventdb']],
                         CLEAN_SCHEME_NAME,
                         row[['classification']],
                         row[['class']],
                         row[['model']],
                         TOOL_NAME,
                         DEPVAR_NAME)

          message(note)

          esdata %>>%
            subset(type %in% c('control',TOOL)) %>>%
            subset(iso3 %in% row[['class_countries']][['iso3']]) %>>%
            subset(
            (.ix %in% CLEAN_SCHEME) |
              (.ix %like% "^C")
            ) ->
            data

          try({es_cegr_method(
            data = data,
            yva = DEPVAR,
            xvars = 'pscore',
            tte_var = tte_var,
            fe = fe,
            window = window,
            shift = shift
          )}) -> ES_RESULT

          if ('try-error' %in% class(ES_RESULT)){
            ES_RESULT <- NULL
          }

          data.table(
            caseid = note,
            eventdb = row[['eventdb']],
            classification = row[['classification']],
            class = row[['class']],
            model = row[['model']],
            tool = TOOL_NAME,
            depvar = DEPVAR_NAME,
            yvar = list(ES_RESULT[['yvar']]),
            xvars = list(ES_RESULT[['xvars']]),
            fe = list(ES_RESULT[['fe']]),
            shift = list(ES_RESULT[['shift']]),
            window = list(ES_RESULT[['window']]),
            felm.obj = list(ES_RESULT[['felm.obj']]),
            felm.estimates = list(ES_RESULT[['felm.estimates']]),
            felm.wald = list(ES_RESULT[['felm.wald']]),
            plotting_data = list(ES_RESULT[['plotting_data']]),
            plot = list(ES_RESULT[['plot']])
          )
        }) %>>% rbindlist
      }) %>>% rbindlist
    }) %>>% rbindlist
  }) %>>% rbindlist -> RESULTS_STAGE2


