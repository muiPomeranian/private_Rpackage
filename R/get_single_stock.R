library(roxygen2)
#' get the single stock from source(only closing price information)
#' @param start
#' @param end
#' @param source_name
#' @param ticker_name
#' @return closestockprice
#' @export
#' @examples stock_1 = get_single_Stock('2001-03-12','2003-04,22','AAPL','yahoo')
#' @details this function get the <<single>> stock information from source, default true for getting close price. You can customize the source(where to get the data)

get_single_stock = function(start,end,ticker_name, source_name)
{
  # this create the new environment to store the result
  sp500 <- new.env()
  getSymbols(ticker_name, env = sp500, src = source_name, from = start, to = end)
  return(sp500[[ticker_name]][,4])
}
