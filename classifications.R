##' @export
classify_mkt.vs.bank <- function(iso3){
  o = rep(NA,times = length(iso3)) %>>% as.character

  o[(iso3 %in% c("AUS" ,"CHE","CHL","FIN","GBR","HKG","IND","JOR","KOR","KWT","PAK","PER","PNG","QAT","RUS","SAU","SGP","SLV","SWE","TTO","TUR","USA","ZAF","ZWE"))] <- 'Market-based'

  o[is.na(o)] <- 'Bank-based'

  return(o)
}

##' @export
classify_price.vs.quantity <- function(x){
  o = rep(NA,times = length(x)) %>>% as.character
  o[x %in% c('ltv_cap','ibex','rr_local','rr_foreign','concrat')] <- 'Quantity'
  o[is.na(o)] <- "Price"

  return(o)
}

##' @export
classify_price.vs.quantity_robustness_1 <- function(x){
  o = rep(NA,times = length(x)) %>>% as.character
  o[x %in% c('ltv_cap','ibex','concrat')] <- 'Quantity'
  o[is.na(o)] <- "Price"

  return(o)
}

##' @export
classify_price.vs.quantity_robustness_2 <- function(x){
  o = rep(NA,times = length(x)) %>>% as.character
  o[x %in% c('ltv_cap')] <- 'Quantity'
  o[is.na(o)] <- "Price"

  return(o)
}

##' @export
classify_capital.vs.other <- function(x){
  o = rep(NA,times = length(x)) %>>% as.character
  o[x %in% c('sscb_res','sscb_cons','sscb_oth','sscb','cap_req')] <- 'Capital'
  o[is.na(o)] <- "Non-Capital"

  return(o)
}

##' @export
classify_borrower.vs.other <- function(x){
  o = rep(NA,times = length(x)) %>>% as.character
  o[x %in% c('ltv_cap')] <- 'borrower'
  o[is.na(o)] <- "institution"

  return(o)
}
