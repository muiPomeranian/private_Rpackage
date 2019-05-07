library(roxygen2)
library(factoextra)
#' get summary of current market at given period
#' @param stock_bunch stacked or one matrix of multiple stock informations with only one single price information(not all OHLC)

#' @return summary of principle component analysis result
#' @export
#' @examples give_summary(stock_bunch)
#' @details this gives all summary of PCA analysis information result for the market

# this gives the summary analyze of given bunch of stocks
give_summary = function(stock_bunch)
{
  temp_pca = prcomp(stock_bunch, center = TRUE ,scale. = TRUE)
  summary(temp_pca)
}
