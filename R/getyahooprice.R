library(roxygen2)
#' get the each single stock corresponding to the ticker name from yahoo finance(OHLC information)
#' @param start
#' @param end
#' @param ticker_name
#' @return stock of OHLC
#' @export
#' @examples stock_1 = getyahooprice('MSFT',2001-03-12','2003-04,22')
#' @details this function get the <<single>> stock information from source, default true for getting close price from yahoo finance. Not like get_single_stock, it also gives all OHLC information of stock

getyahooprice = function(tickername, start, end)
{
  start <- start
  end <- end
  ticker_name = tickername
  source_name = 'yahoo'
  a = get_single_stock(start,end,ticker_name, source_name) # if we run this function, ticker_name will be saved
  return(a)
}
