library(roxygen2)

#' functions to choose best stock to invest based on strategy
#' @param output_variable, stocks that will be invested
#' @param num_companies chosen numbers what user want to have as a potential investment stock

#' @return return the determined stock features based on strategy chosen
#' @export
#' @examples determine_portfolio(output_variable, num_companies)
#' @details
#' for each date or quarter, we sort the data and select top , then give out the determined best top(chosen num_compnies) stock to be invested


## here we input in the function a datatable with the momentum
## for each date or quarter, we sort the data and select top
determine_portfolio = function(output_variable, num_companies)
{

  portfolio = c()
  for(j in 1:nrow(output_variable))
  {
    print(j)
    seli = sort(as.data.frame(output_variable[j,]),decreasing = T)[1:num_companies]
    portfolio = rbind(portfolio,names(seli))
  }
  portfolio = xts(portfolio, order.by =time(output_variable) )
  return(portfolio)
}

