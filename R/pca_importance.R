#' get pca result importance using standard deviation and variances
#' @param pca_result_vector

#' @return importance of varaince analysis
#' @export
#' @examples pca_importance(pca_result_vector)
#' @details this function is to transform a PCA type output into a regular data frame to it can be used in Shiny as a DT output


pca_importance = function(x) {
  vars <- x$sdev^2
  vars <- vars/sum(vars)
  rbind(`Standard deviation` = x$sdev, `Proportion of Variance` = vars,
        `Cumulative Proportion` = cumsum(vars))
}
