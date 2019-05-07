library(roxygen2)
#' draw multiple plots based on rolling regression prediction
#' @param target single stock
#' @param rolled_input calculated from function execute_roll_linear

#' @return draws plots
#' @export
#' @examples prediction_matrix= draw_comparison_result_plot(stock, rolled_input)
#' @details draws the plot of showing all upper,lower,middle boundary prediction for each given period. It will compute the all necessary tool to draw the rolling sliding window based linear regression, it will draw the line for each corresponding purpose, as commented

# below line will compute the all necessary tool to draw the rolling sliding window based linear regression
# it will draw the line for each corresponding purpose, as commented
draw_comparison_result_plot = function(input,rolled_input){
  # rolled_input = execute_roll_linear(input,window_size)
  plot(index(input), input, type="l", lwd=2, col = 'black', las=1 ) # real plot
  lines(index(rolled_input), rolled_input$fit, col="orange", lwd=5 ) # final prediction line
  lines(index(rolled_input), rolled_input$lwr, col="purple", lwd=3, lty=3 )
  lines(index(rolled_input), rolled_input$upr, col="purple", lwd=3, lty=3 )
  abline(lm(Cl(input) ~ index(input)), col="red", lwd=3 )  # this is the line result from linear regression
  legend("topleft", legend = c('Return: Black', 'Prediction: Orange','Lower bound: Purple', 'Upper bound: Purple', 'Reg line: Red') )
}
