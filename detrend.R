##' @import foreach doMC
##' @export
detrend <- function(data,idcol,xcol,ycol,
                    pre_bound = 0,
                    ncpu = 4){
  registerDoMC(ncpu)

  data %>>%
    copy %>>%
    setnames(
      old = c(idcol,xcol,ycol),
      new = c('id','x','y')
    ) %>>%
    select(id,x,y) %>>%
    subset(!is.na(y)) %>>%
    split(.[['id']]) ->
    l.data

  l.detrend <- list()
  foreach(dt = l.data,
          .errorhandling = 'pass') %dopar% {
            dt %>>%
              subset( x < pre_bound) %>>%
              lm(formula = y ~ x) ->
              o

            .hat <- predict.lm(o,newdata = dt, se.fit = TRUE)

            dt %>>%
              mutate(
                hat = .hat[['fit']],
                err = y - hat,
                resid.sd = .hat[['residual.scale']]
              ) %>>%
              data.table
          } %>>% Filter(f = is.data.table) %>>% rbindlist ->
  l.detrend[['pre-period']]

  foreach(dt = l.data,
          .errorhandling = 'pass') %dopar% {
            dt %>>%
              lm(formula = y ~ x) ->
              o

            .hat <- predict.lm(o,newdata = dt, se.fit = TRUE)

            dt %>>%
              mutate(
                hat = .hat[['fit']],
                err = y - hat,
                resid.sd = .hat[['residual.scale']]
              ) %>>%
              data.table
          } %>>% Filter(f = is.data.table) %>>% rbindlist ->
  l.detrend[['all-period']]

  l.detrend %>>%
    list.map({
      . %>>%
        copy %>>%
        setnames(
          old = c('id','x','y','hat','err','resid.sd'),
          new = c(idcol,xcol,ycol,
                  sprintf('%s_hat',ycol),
                  sprintf('%s_err',ycol),
                  sprintf('%s_resid.sd',ycol))
        )
    }) ->
    out

  return(out)
}



