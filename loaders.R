##' @export
loader <- function(event_type = NULL){

  event_types = c('quantity','price','all','sector','activity')

  if (is.null(event_type)){
    stop("Specify one of the following: ", paste(event_types,collapse = ", " ))
  }

  if (length(event_type)>1){
    stop("Only one event type can be selected at a time.")
  }

  switch(
  {event_type},
  'quantity' = 'MPRUD_QUANTITY',
  'price' = 'MPRUD_PRICE',
  'all' = 'MPI',
  'activity' = 'MPRUD_ACTIVITY',
  'sector' = 'MPRUD_SECTOR'
  ) %>>%
    sprintf(fmt = 'dataset_expand_%s') ->
    dataset_name

  sprintf(fmt = '%s/%s.RData',
          dir.data,
          dataset_name) ->
    FILE

  load(FILE)

  message(sprintf("Loaded the dataset '%s'",dataset_name))

  dt = get(dataset_name) %>>% data.table

  class(dt) = c(class(dt),'esdataset')
  return(dt)
}

