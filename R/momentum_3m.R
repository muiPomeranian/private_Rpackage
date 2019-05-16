library(roxygen2)

#' functions related to momentum calculation
#' @param days_end ending date
#' @param freq chosen frequency at given observation

#' @return return the momentum features with 3 months window size
#' @export
#' @examples momentum_3m(dataframe, dates_review,freq)
#' @details
#' for each quarter we compute a momentum value


momentum_3m = function(dataframe, dates_review,freq)
{
  freq = as.numeric(freq)
  output_m = c()
  mins = 1
  for(x in length(dates_review):2)
  {
    print(dates_review[x])
    prices_x = dataframe[dates_review[x],]
    prices_x_3 = dataframe[dates_review[x-mins],]
    res = as.data.frame(t(as.numeric(prices_x)/as.numeric(prices_x_3)))
    res = xts(res,order.by = time(prices_x))
    colnames(res) = colnames(prices_x)
    output_m = rbind(output_m,res)
  }
  return(output_m)
}
