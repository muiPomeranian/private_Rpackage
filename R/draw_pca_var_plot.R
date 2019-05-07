library(roxygen2)
library(factoextra)
#' # draws result of the variance analysis from pca
#' @param stock_bunch stacked or one matrix of multiple stock informations with only one single price information(not all OHLC)

#' @return pca scatter plot
#' @export
#' @examples draw_pca_var_plot(stock_bunch)
#' @details this gives result by scatter plot result of PCA analysis information for current market in given period. Shows the relationship of each period for each stock

# below will draw the variance comparison plot to give an idea of which stock is more important to explain market of SnP
draw_pca_var_plot = function(stock_bunch)
{
  temp_pca = prcomp(stock_bunch, center = TRUE ,scale. = TRUE)

  fviz_pca_ind(temp_pca,
               col.ind = "cos2", # Color by the quality of representation
               gradient.cols = c("#00AFBC", "#E7B800", "#FC4E07"),
               repel = TRUE     # Avoid text overlapping
  )
}
