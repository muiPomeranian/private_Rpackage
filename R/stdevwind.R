# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

library(roxygen2)
#' rolling standard deviation function
#'
#' @param stockreturns
#' @return anualized volatility
#' @export
#' @examples stdevwind(stock1)
#' @details this will calculated the standard deviation based on sliding window.Also, it will return the annulized standard deviation based on market live day out of 365 days in a year. calculate adjusted window frame for anualized version of sliding window based standard deviation
stdevwind = function(returns)
{
  st_dev_df = c()
  for (i in 253:dim(returns)[1])
  {
    wind = window(returns, start = time(returns)[i-251], end = time(returns)[i])
    stdv = StdDev.annualized(wind, scale  = 252)
    st_dev_df = rbind(st_dev_df,stdv)

  }
  st_dev_df = xts(st_dev_df, order.by = time(returns)[253:length(time(returns))])

  return(st_dev_df)

}
