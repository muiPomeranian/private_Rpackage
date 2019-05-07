library(roxygen2)
#' Stack each stock(what User decided to wrangle together) as a columnwise
#' @param start
#' @param end
#' @param list of ticker_names
#' @return multiple stacked stocks of closing price information
#' @export
#' @examples stock_stack= get_multiple_stock(list_stocks,2001-03-12','2003-04,22')
#' @details this function get the <<multiple>> stock closing price informations from source, default true for getting close price from yahoo finance.


get_multiple_stock = function(list_stocks,start, end)
{
  df_output = c()

  for(i in 1:length(list_stocks))
  {
    df_output = cbind(df_output, getyahooprice(list_stocks[i],start,end))
  }
  return(df_output)
}

