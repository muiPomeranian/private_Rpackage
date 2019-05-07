library(roxygen2)
#' predicted value from rolling based regression
#' @param target single stock
#' @param rolling window size

#' @return matrix of predicted value,and uppder,lower boundary
#' @export
#' @examples prediction_matrix= execute_roll_linear(stock,30)
#' @details this function will return the rolled_input return value which contains 1) upper boundary preiction, 2)lower boundary prediction, and 3) middle value final prediction for each given period. This can be used as a one guideline to set as bench mark model before using any other prediction models from machine/deep learning

execute_roll_linear = function(a, window_size){
  # this is the function for rollapplyr's FUN
  pred_fun = function(x) {
    r <- lm( x ~ index(x) )
    tail(predict(r, interval="prediction"),1)
  }
  # Rolling regression (unweighted), with prediction intervals
  rolled_input <- rollapplyr(
    as.zoo(Cl(a)),
    width=window_size, by.column = FALSE,
    FUN = pred_fun
  )
  return(rolled_input)
}
