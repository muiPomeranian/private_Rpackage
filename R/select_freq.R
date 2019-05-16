library(roxygen2)

#' function to select quarterly data..
#' @param days_end ending date
#' @param freq chosen frequency at given observation

#' @return return the quteraly data based on end date and frequency
#' @export
#' @examples select_freq(days_end,freq)
#' @details
#' function to select quarterly data from stored data


## function to select quarterly data
select_freq = function(days_end,freq)
{
  freq = as.numeric(freq)
  ## select dates for which the month is a multiple of 3
  return(days_end[  which((month(days_end)%%freq)==0)])

}
