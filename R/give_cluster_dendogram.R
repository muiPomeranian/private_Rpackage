#' draw k-mean cluster analysis result in histogram table
#' @param df_stocks bunch of stocks stacked which user want to analyze

#' @return dendogram plot result from hierarchical cluster models
#' @export
#' @examples draw_cluster_analysis(df_stocks, num_center, num_start)
#' @details by comparing the plot result from this function,User can analysis the stock markets regime. For instance, for each plot shows how stock movesment behaves differently. User could see the momentum of effectiveness in the market and means of the variables in each cluster. below function is another way to show the similiarities between each designated stocks User choose. Risk parity model can be combined with this. This could be another indicator of similarities of stock movement in the market at given time period

give_cluster_dendogram = function(df_stocks){
  df = t(df_stocks)
  d = dist(df)
  hcl = hclust(d)
  plot(hcl)
}












