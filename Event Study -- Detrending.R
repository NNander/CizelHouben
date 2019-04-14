load_all()

require(data.table)
require(dplyr)
require(dtplyr)
require(pipeR)
require(rlist)
require(scales)
require(pracma)

#country_classification() ->
 # ref.ctry

ref.ctry[['All']] <- ref.ctry[['AE_EME.IMF']] %>>% mutate(region = 'All')

#batch_preparation_yxdata() ->
 # l.yxdata

#werkt
imfutils::imfRefCtry() %>>%
  select(iso3,name) %>>%
  setkey(iso3) ->
  ref.iso3

#'inst/data',
#'
vintage<-0;
import_data <- function(){
  infile = file.path("/Users/Nander/Desktop/Cer",
                     sprintf('Event study data.RData',
                             vintage))

  load(infile)
}

import_data() ->
  l.events

l.events <- readRDS("~/Desktop/Cer/l.events.rds")

prepare_ydata.oldnew.levels() ->
  ydata

#werkt
.cleaner <- function(x){
  x %>>%
    (ifelse(is.infinite(.),NA,.)) %>>%
    (ifelse(is.nan(.),NA,.))
}

#options(dir.root = "/Users/Nander/Desktop/Cer")

#werkt
l.events %>>% list.map({
  EVENTDB_NAME = .name
  l.event_data = .

  (ydata %>>%
     setkey(iso3,date))[
       l.event_data[['event_db']][['event_panel_expanded']] %>>%
         setkey(iso3,date)
       ] %>>%
    subset(
      ! iso3 %in% c('ARG','GRC','SAU','MYS')
    ) ->
    esdata

  data.table(
    eventdb = EVENTDB_NAME,
    eventdb_obj = list(l.event_data),
    esdata = list(esdata)
  )
}) %>>% rbindlist -> l.esdata

## -------------------------------------------------------------------------- ##
## Transform yseries to cumulative changes around event date (Y_t/Y_0)        ##
## -------------------------------------------------------------------------- ##

#Werkt
shift = 0

l.esdata %>>% apply(1, function(row){
  esdata = row[['esdata']]

  esdata %>>%
    dplyr::select(
      .ix,tte,
      one_of(setdiff(names(ydata),c('iso3','date')))
    ) %>>%
    melt(
      id.vars = c('.ix','tte')
    ) %>>%
    mutate(
      tte_shift = tte + shift
    ) %>>%
    subset(
      !is.na(value)
    ) ->
    esdata.ydata.long

  esdata.ydata.long %>>%
    subset(
      tte_shift == -1
    ) %>>%
    select(
      .ix,variable,value_0 = value
    ) ->
    esdata.ydata.t0


  (esdata.ydata.t0 %>>%
     setkey(.ix,variable))[
       esdata.ydata.long %>>%
         setkey(.ix,variable)
       ] %>>%
    mutate(
      cumchg = (value / value_0) %>>% .cleaner
    ) %>>%
    subset(
      tte_shift %between% c(-10,10)
    ) %>>%
    dcast(
      .ix + tte_shift ~ variable,
      value.var = 'cumchg'
    ) ->
    esdata.final

  data.table(
    eventdb = row[['eventdb']],
    eventdb_obj = list(row[['eventdb_obj']]),
    esdata = list(row[['esdata']]),
    esdata_rescaled = list(esdata.final)
  )
}) %>>% rbindlist ->
  l.esdata

l.depvars <- list(
  `Banks_LCU` = "pchgy_banks_lcu",
  `Nonbanks_LCU` = "pchgy_nonbanks_lcu",
  `All_LCU` = "pchgy_all_lcu",
  `Banks_USD` = "pchgy_banks_usd",
  `Nonbanks_USD` = "pchgy_nonbanks_usd",
  `All_USD` = "pchgy_all_usd"
)

ycols = l.depvars %>>% unlist %>>% as.character



## TOT HIER GEBLEVEN
## DETRENDING OPERATION
l.esdata %>>% apply(1, function(row){
  esdata.final = row[['esdata_rescaled']]

  require(doMC)
  l.detrend <- list()
  for (ycol in ycols){
    message(ycol)
    detrend(
      esdata.final,
      idcol = '.ix',
      xcol = 'tte_shift',
      ycol = ycolx
    )[['pre-period']] ->
      l.detrend[[ycol]]
  }

  l.detrend %>>%
    list.map({
      . %>>%
        mutate(var = .name) %>>%
        melt(
          id.vars = c('.ix','var','tte_shift')
        )
    }) %>>%
    rbindlist %>>%
    mutate(
      trans = ifelse(
        variable %like% '_hat',
        'fit',
        ifelse(
          variable %like% '_err',
          'error',
          ifelse(
            variable %like% '_resid.sd',
            'residual_stdev',
            'level'
          )
        )
      )
    ) %>>%
    select(
      -variable
    )->
    data.detrended.long

  row[['data_detrended']] <- data.detrended.long

  data.table(
    eventdb = row[['eventdb']],
    eventdb_obj = list(row[['eventdb_obj']]),
    esdata = list(row[['esdata']]),
    esdata_rescaled = list(row[['esdata_rescaled']]),
    data_detrended = list(row[['data_detrended']])
  )
}) %>>% rbindlist ->
  l.esdata



## -------------------------------------------------------------------------- ##
## FIND TREATMENT CASES IN WHICH THE AVERAGE POST-EVENT RESPONSE BY BANKS WAS ##
## NEGATIVE. THIS SHOULD INDICATE THAT MAP'S WERE EFFECTIVE                   ##
## -------------------------------------------------------------------------- ##
l.esdata %>>% apply(1,function(row){
  row[['eventdb_obj']][['event_db']][['treatment_lookup']] ->
    df_eventstudy_lookup

  row[['data_detrended']] %>>%
    copy %>>%
    subset(trans == 'error') %>>%
    subset(tte_shift %between% c(0,8)) %>>%
    (dt ~ dt[,{
      list(
        reaction = sign(mean(value,na.rm = TRUE))
      )
    }, by = c('.ix','var')]) %>>%
    na.omit %>>%
    dcast(
      .ix ~ var, value.var = 'reaction'
    )->
    event_reactions

  (df_eventstudy_lookup %>>%
     setkey(.ix))[
       event_reactions %>>%
         setkey(.ix)
       ] ->
    event_reactions

  l.cases <- list()

  event_reactions %>>%
    subset(banks_lcu == -1) %>>%
    (.ix) ->
    l.cases[['Decline in bank credit']]

  event_reactions %>>%
    subset(nonbanks_lcu == 1) %>>%
    (.ix) ->
    l.cases[['Growth in nonbank credit']]

  event_reactions %>>%
    (.ix) ->
    l.cases[['Unconditional response']]

  data.table(
    eventdb = row[['eventdb']],
    eventdb_obj = list(row[['eventdb_obj']]),
    esdata = list(row[['esdata']]),
    esdata_rescaled = list(row[['esdata_rescaled']]),
    data_detrended = list(row[['data_detrended']]),
    cases = list((l.cases))
  )
}) %>>% rbindlist ->
  l.esdata

l.esdata %>>% apply(1,function(row){
  row[['eventdb_obj']][['event_db']][['treatment_lookup']] ->
    df_eventstudy_lookup

  row[['data_detrended']] ->
    data.detrended.long

  (df_eventstudy_lookup %>>%
     setkey(.ix))[
       data.detrended.long %>>%
         setkey(.ix)
       ] ->
    data.detrended.long2

  data.detrended.long2 %>>%
    mutate(
      type = ifelse(is.na(type), 'control',as.character(type))
    ) ->
    dataset

  data.table(
    eventdb = row[['eventdb']],
    eventdb_obj = list(row[['eventdb_obj']]),
    esdata = list(row[['esdata']]),
    esdata_rescaled = list(row[['esdata_rescaled']]),
    data_detrended = list(row[['data_detrended']]),
    cases = list(row[['cases']]),
    dataset = list(dataset)
  )
}) %>>% rbindlist ->
  l.esdata

l.class_quantprice <- list(
  'All' = 'All',
  'Quantity' = 'Quantity',
  'Price' = 'Price'
)

l.esdata %>>% apply(1,function(row){
  EVENTDB_NAME = row[['eventdb']]
  EVENTDB = row[['eventdb_obj']]
  dataset = row[['dataset']]
  EVENTDB[['Clean overlapping events']] %>>% list.map({
    CLEAN_SCHEME_NAME = .name
    CLEAN_SCHEME = . %>>% (.ix) %>>% unique
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
          l.class_quantprice %>>% list.map({
            TOOL_NAME = .name
            TOOL = .
            l.depvars %>>% list.map({
              DEPVAR_NAME = .name
              DEPVAR = .
              note = sprintf("CEGR - Detrended | %s | %s | %s | %s | %s | %s",
                             EVENTDB_NAME,
                             CLEAN_SCHEME_NAME,
                             CLASSIFICATION_NAME,
                             CLASS_NAME,
                             TOOL_NAME,
                             DEPVAR_NAME)

              outputs <- list()

              message(note)

              ## ANALYSIS
              dataset %>>%
                subset(trans == 'error') %>>%
                subset(var == DEPVAR) %>>%
                subset(type %in% TOOL) %>>%
                subset(iso3 %in% CLASS[['iso3']]) %>>%
                subset(
                (.ix %in% CLEAN_SCHEME) |
                  (.ix %like% "^C")
                ) ->
                data

              data %>>%
                es_detrended_method ->
                outputs[["ES"]]

              outputs[['Note']] <- note
              outputs[['Note - short']] <- note

              ## REPORT COUNTRY-YEAR TREATMENT CASES
              (ref.iso3 %>>% setkey(iso3))[
                data %>>% select(iso3,.ix,mid) %>>%
                  setkey(iso3)
                ] %>>%
                setkey(.ix) %>>%
                unique %>>%
                mutate(
                  .print. = sprintf("%s (%s)",
                                    name,
                                    year(mid))
                ) ->
                outputs[['Treatment cases']]

              outputs

              data.table(
                caseid = note,
                eventdb = EVENTDB_NAME,
                classification = CLASSIFICATION_NAME,
                class = CLASS_NAME,
                tool = TOOL_NAME,
                depvar = DEPVAR_NAME,
                yvar = DEPVAR,
                shift = shift,
                ES = list(outputs[['ES']]),
                treatment_cases = list(outputs[['Treatment cases']])
              )
            }) %>>% rbindlist
          }) %>>% rbindlist
        }) %>>% rbindlist
    }) %>>% rbindlist
  }) %>>% rbindlist
}) %>>% rbindlist ->
  l.RESULTS


## CONDITION ON THE BANK RESPONSE
l.class_quantprice %>>% list.map({
  TOOL_NAME = .name
  TOOL = .
  l.sample %>>% list.map({
    SAMPLE_NAME = .name
    SAMPLE = .
    l.depvars %>>% list.map({
      DEPVAR_NAME = .name
      DEPVAR = .

      outputs <- list()
      note = sprintf("Depvar - %s | Sample - %s | Tool - %s",
                     DEPVAR_NAME,
                     SAMPLE_NAME,
                     TOOL_NAME)

      note_short = sprintf("%s|%s|%s",
                           DEPVAR_NAME,
                           SAMPLE_NAME,
                           TOOL_NAME)

      message(note)

      ## ANALYSIS
      dataset %>>%
        subset(trans == 'error') %>>%
        subset(var == DEPVAR) %>>%
        subset(type %in% TOOL) %>>%
        subset(region.imf %in% SAMPLE) %>>%
        subset(.ix %in% l.cases[['Decline in bank credit']])->
        data

      data %>>%
        es_detrended_method ->
        outputs[["ES"]]

      outputs[['Note']] <- note
      outputs[['Note - short']] <- note_short

      ## REPORT COUNTRY-YEAR TREATMENT CASES
      (ref.iso3 %>>% setkey(iso3))[
        data %>>% select(iso3,.ix,mid) %>>%
          setkey(iso3)
        ] %>>%
        setkey(.ix) %>>%
        unique %>>%
        mutate(
          .print. = sprintf("%s (%s)",
                            name,
                            year(mid))
        ) ->
        outputs[['Treatment cases']]

      outputs
    })
  })
}) ->
  l.RESULTS_NEGBANK
