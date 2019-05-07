library(roxygen2)
#' get rmse score result
#' @param pred prediction result from any model
#' @param original original value at given period

#' @return rmse score
#' @export
#' @examples rmse= get_rmse(pred,original)
#' @details this will give rmse calculated score and this automatically handles the different window size of input data

# this will give rmse calculated score
# this automatically handles the different window size of input data
get_rmse = function(pred, original){
  original = window(original, start = start(pred))
  return(sqrt(mean( (pred - original)^2 ) ) )
}
