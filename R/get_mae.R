library(roxygen2)
#' get MAE score result
#' @param pred prediction result from any model
#' @param original original value at given period

#' @return MAE score
#' @export
#' @examples mae= get_mae(pred,original)
#' @details this will give rmse calculated score and this automatically handles the different window size of input data

# this will give MAE calculated score
# this automatically handles the different window size of input data
get_mae = function( pred, original){
  original = window(original, start = start(pred))
  return(sqrt(mean( abs(pred - original)) ) )
}
