##' @import openxlsx readxl pipeR data.table dplyr lubridate
##' @export
prepare_MaP_events <- function(){
 #  FILE.XL <- system.file('./data/original/prudential_ind_1.xlsx',
  #                         package = 'Projects2016.Macropru')
  
    FILE.XL <- '/Users/Nander/Desktop/inst-data-original/prudential_ind_1.xlsx'
    read_excel(FILE.XL) %>>%
        data.table %>>%
        mutate(
            date = sprintf("%s-%s-%s",
                           year,
                           quarter*3,
                           1) %>>%
                as.Date %>>%
                (x ~ x + months(1) - 1)
        ) %>>%
        select(
            - year,
            - quarter,
            - qdate,
            - country,
            - biscode
        ) %>>%
        melt.data.table(
            id.vars = c('ifscode','date')
        ) %>>%
        subset(
            value != 0 &
                (! variable %like% "cum") &
                (! variable %like% "int") &
                (! variable %like% "PruC")
        ) %>>%
        mutate(
            type = ifelse(value>0,"Tighten","Relax")
        ) ->
        mapevents

    return(mapevents)
}

##' @export
prepare_MaP_events_update <- function(){
#    FILE.XL <- system.file('./data/original/Prudential policy instruments_new/prudential_ind_3.xlsx',
 #                          package = 'Projects2016.Macropru')
    
  FILE.XL <- '/Users/Nander/Desktop/inst-data-original/prudential_ind_3.xlsx'

    read_excel(FILE.XL) %>>%
        data.table %>>%
        mutate(
            date = sprintf("%s-%s-%s",
                           year,
                           quarter*3,
                           1) %>>%
                as.Date %>>%
                (x ~ x + months(1) - 1)
        ) %>>%
      data.table %>>%
      select(
            - year,
            - quarter,
            - qdate,
            - country,
            - biscode
        ) %>>%
        melt.data.table(
            id.vars = c('ifscode','date')
        ) %>>%
        subset(
            value != 0 &
                (! variable %like% "cum") &
                (! variable %like% "int") &
                (! variable %like% "PruC") &
                (! variable %like% "core_country")
        ) %>>%
        mutate(
            type = ifelse(value>0,"Tighten","Relax")
        ) ->
        mapevents

    return(mapevents)
}

prepare_MaP_events() ->   events
