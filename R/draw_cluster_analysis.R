#' draw k-mean cluster analysis result in histogram table
#' @param df_stocks bunch of stocks stacked which user want to analyze
#' @param num_center number of centers of clusters user might choose. There are no statisticaly proven best number. But You can use elbow-method to choose best param
#' @param num_start number of random initialized starting point as a initial poitn of model training

#' @return plot of cluster result from k-means
#' @export
#' @examples draw_cluster_analysis(df_stocks, num_center, num_start)
#' @details This compute the k-means algorithm with user designed parameter. Then it will draw the histogram result. This can be used to analyze the market trend, similiarity, risk parity, and regime of the market movement for each stocks inputted

draw_cluster_analysis = function(df_stocks, num_center, num_start){
  df_scaled_stocks = scale(df_stocks)
  km_model = kmeans(df_scaled_stocks,centers = num_center, nstart = num_start)
  centers = data.frame(cluster = factor(1:num_center), km_model$centers)
  centers = as.data.frame(t(centers))
  names(centers) = paste("Cluster",1:num_center)
  centers$Symbol = row.names(centers)
  centers = gather(centers, "Cluster", "Mean", -Symbol)
  centers$Coloar = centers$Mean > 0
  ggplot(centers, aes(x=Symbol, y=Mean, fill = 'red')) + geom_bar(stat = 'identity', position = 'identity', width = 0.80)+
    facet_grid(Cluster ~., scales = 'free_y')
}

