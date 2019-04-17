load_all()

#Works
# 'inst/data' 2nd line
import_data <- function(){
  infile = file.path("/Users/Nander/Desktop/Cer",
                     sprintf('Event study data.RData',
                             vintage))
  load(infile)
}

#
#import_data() ->
 # l.events

#does not work 
country_classification() ->
  ref.ctry

#Works
ref.ctry[['All']] <- ref.ctry[['AE_EME.IMF']] %>>% mutate(region = 'All')

#Does not work
#batch_preparation_yxdata() ->
 # l.yxdata

#Works
l.events %>>% list.map({
  EVENTDB_NAME = .name
  l.event_data = .

  (l.yxdata[['data_wide']] %>>%
     setkey(iso3,date))[
       l.event_data[['event_db']][['event_panel_expanded']] %>>%
         setkey(iso3,date)
       ] %>>%
    subset(
      ! iso3 %in% c('ARG','GRC','SAU','MYS')
    ) %>>%
    mutate(
      type = ifelse(is.na(type), 'control',as.character(type)),
      region.wb = ifelse(iso3 %in% ref.ctry[['AE_EME.WB']][region == 'AE'][['iso3']],
                         'AE','EME'),
      region.imf = ifelse(iso3 %in% ref.ctry[['AE_EME.IMF']][region == 'AE'][['iso3']],
                          'AE','EME'),
      region.mktbk = ifelse(iso3 %in% ref.ctry[['MKT_BK']][mktbkclass == 'Market-based'][['iso3']],
                            'Market-based','Bank-based')
    ) ->
    esdata

  data.table(
    eventdb = EVENTDB_NAME,
    eventdb_obj = list(l.event_data),
    esdata = list(esdata)
  )
}) %>>% rbindlist -> l.esdata

#Works
l.depvars <- list(
  `Banks_LCU` = "pchgy_banks_lcu",
  `Nonbanks_LCU` = "pchgy_nonbanks_lcu",
  `All_LCU` = "pchgy_all_lcu",
  `Banks_USD` = "pchgy_banks_usd",
  `Nonbanks_USD` = "pchgy_nonbanks_usd",
  `All_USD` = "pchgy_all_usd"
)

#Works
l.class_quantprice <- list(
  'All' = 'All',
  'Quantity' = 'Quantity',
  'Price' = 'Price'
)

#Works
l.xvars <- list(
  spec1 = c('PCPIPCH', 'NGDP_RPCH', 'FILR','GFDD.OI.19',"factor(date)"),
  spec2 = c('PCPIPCH', 'NGDP_RPCH', 'FILR','GFDD.OI.19')
)

#Works
window = c(-8,8) #c(-12,9)
shift = 4
tte_var = 'factor(tte)'
xvars = l.xvars[[2L]]
fe = 'factor(date)'

#Worked (once)
l.esdata %>>% apply(1,function(row){
  EVENTDB_NAME = row[['eventdb']]
  EVENTDB = row[['eventdb_obj']]
  esdata = row[['esdata']]

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
              
              note = sprintf("CEGR | %s | %s | %s | %s | %s | %s",
                             EVENTDB_NAME,
                             CLEAN_SCHEME_NAME,
                             CLASSIFICATION_NAME,
                             CLASS_NAME,
                             TOOL_NAME,
                             DEPVAR_NAME)

              message(note)

              esdata %>>%
                subset(type %in% c('control',TOOL)) %>>%
                subset(iso3 %in% CLASS[['iso3']]) %>>%
                subset(
                (.ix %in% CLEAN_SCHEME) |
                  (.ix %like% "^C")
                ) ->
                data
              
              try({es_cegr_method(
                data = data,
                yvar = DEPVAR,
                xvars = xvars,
                tte_var = tte_var,
                fe = fe,
                window = window,
                shift = shift
              )}) ->> ES_RESULT

              if ('try-error' %in% class(ES_RESULT)){
                print('try error occured')
                ES_RESULT <- NULL
              }
              

              data.table(
                caseid = note,
                eventdb = EVENTDB_NAME,
                classification = CLASSIFICATION_NAME,
                class = CLASS_NAME,
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
    }) %>>% rbindlist
  }) %>>% rbindlist
}) %>>% rbindlist ->
  RESULTS


