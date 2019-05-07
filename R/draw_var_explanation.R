library(roxygen2)
#' # draws result of the variance analysis
#' @param stock_bunch stacked or one matrix of multiple stock informations with only one single price information(not all OHLC)

#' @return histogram of varaince analysis
#' @export
#' @examples draw_var_explanation(stock_bunch)
#' @details this gives result by histogram table of PCA analysis information for current market in given period

draw_var_explanation = function(stock_bunch){
  temp_pca = prcomp(stock_bunch, center = TRUE ,scale. = TRUE)
  res = fviz_eig(temp_pca)
  return(res)
}
