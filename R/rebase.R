library(roxygen2)
#' rescale/rebase the given input
#' @param vector
#' @param scale
#' @return rdata
#' @export
#' @examples ## Pass a vector and scale that you want to rescale it. rebase(vecotor,10)
#' @details comparing two price series of stocks for a given window, we need to rebase them otherwise, imagine stock 1 has a price of 100 and stock 2 a price of 1000, you cannot put on same chart, and imagine stock price grows to 150 (+50%) while stock 2 grows to 1050 (+5%). we can visualise than only if you rebase to same scale


rebase = function(vector, scale)
{

  f1 = vector[1]
  print(length(vector))
  print(f1)
  for (y in 1:length(vector))
  {
    vector[y] = scale* vector[y]/as.numeric(f1)
  }
  return(vector)
}
