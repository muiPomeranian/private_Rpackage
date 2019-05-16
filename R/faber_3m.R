library(roxygen2)

#' get the faber features(finance features)..
#' @param dataframe multiple stock information
#' @param dates_review chosen period observation
#' @param frequency frequency forthe user chosen

#' @return momentun valuebased on input dataframe stock file, dates chosen, frequency
#' @export
#' @examples faber_3m(stock, dates_review,freq)
#' @details
#' similar to momentum 3m, except we divide by avg price of 3 months ago
#' it returns the momentum value based on those. Called Faber.

faber_3m = function(dataframe, dates_review,freq)
{

  freq = as.numeric(freq)
  output_m = c()
  mins = 1

  for(x in length(dates_review):2)
  {
    print(dates_review[x])
    prices_x = dataframe[dates_review[x],]

    prices_x_3 = window(dataframe, start =dates_review[x-mins] , end = dates_review[x])

    ## using tibble a function to compute the mean of each column using summarise_all
    prices_x_3 = as.data.frame(prices_x_3) %>%
      summarise_all(funs(mean(., na.rm = TRUE)))

    res = as.data.frame(t(as.numeric(prices_x)/as.numeric(prices_x_3)))
    res = xts(res,order.by = time(prices_x))
    colnames(res) = colnames(prices_x)
    output_m = rbind(output_m,res)
  }
  return(output_m)
}
