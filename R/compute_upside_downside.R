library(roxygen2)

#' draw upside trend, downside trend regime of chosen stocks, compared to bench mark stock
#' @param stock single stock information
#' @param benchmark bench mark stock price user choose

#' @return plot of computed upside, downside trend of stocks user choose
#' @export
#' @examples compute_upside_downside(stock, benchmark_stock)
#' @details this function will take the historical prices of a stock and its benchmark and will do following computations.
#' consider returns only positive and ignore all negative returns: compute an historical track as negative days didn't happen
#' similar function for days as if positive days didn't happen
#' this metric allows to see how much you do better during upside markets and downside markets separately

compute_upside_downside = function(stock, benchmark)
{
  dates = time(stock)
  returns_stock = Return.calculate(stock, method = 'discrete')

  upside_stock = returns_stock
  upside_stock[upside_stock<0] = 0

  downside_stock = returns_stock
  downside_stock[downside_stock>0] = 0

  returns_bm = Return.calculate(benchmark, method = 'discrete')

  upside_bm = returns_bm
  upside_bm[upside_bm<0] = 0

  downside_bm = returns_bm
  downside_bm[downside_bm>0] = 0

  for(x in 1:dim(upside_stock)[1])
  {
    if(x==1) {
      upside_stock[x] = 100
      downside_stock[x] = 100
      upside_bm[x] = 100
      downside_bm[x] = 100
    }else{
      upside_stock[x] = as.numeric(upside_stock[x-1]) * as.numeric((1+upside_stock[x]))
      downside_stock[x] = as.numeric(downside_stock[x-1]) * as.numeric((1+downside_stock[x]))
      upside_bm[x] = as.numeric(upside_bm[x-1]) * as.numeric((1+upside_bm[x]))
      downside_bm[x] = as.numeric(downside_bm[x-1]) * as.numeric((1+downside_bm[x]))
    }

  }

  df_output = cbind(upside_stock,upside_bm,downside_stock, downside_bm)
  colnames(df_output) = c('Upside Stock','Upside BM',"Downside Stock", 'Downside Bm')
  return(df_output)

}
