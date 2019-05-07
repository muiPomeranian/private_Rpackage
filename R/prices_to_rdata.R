library(roxygen2)
#' load a large csv file and will cast it as XTS data class after save this as R data format
#' @param filename
#' @return rdata
#' @export
#' @examples this will allow faster loading and calculation. XTS  is special time series package which handles and allow users to compute based on time frame windows.If user want to compute the return between date A and B, use do window(prices, start = A, end = B). xts class data is working for this method. dataframe, is not working since data frame sees a date as a character.

prices_to_rdata  = function(filename)
{
  prices = read.csv(filename)
  dates = prices[,1]
  dates = as.Date(dates, format = '%m/%d/%Y')
  prices = xts(prices[,-1], order.by = dates)
  save(prices, file = 'prices.Rdata')
}

