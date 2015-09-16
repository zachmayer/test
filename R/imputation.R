#' @title Median Imputation for data.frames
#' @description This function takes a data.frame and replaces NA values with the column median
#' @param x A data.frame with missing values to impute
#' @export
medianImpute <- function(x){
  stopifnot(is.data.frame(x))
  if(nrow(x) <= 1){
    stop('x must have at least 2 rows for imputation')
  }
  medians <- sapply(x, median, na.rm=TRUE)
  for(col in names(x)){
    x[[col]][is.na(x[[col]])] <- medians[col]
  }
  return(x)
}

#' @title NA Imputation for data.frames via knn
#' @description This function takes a data.frame and replaces NA values with the column predictions for a knn model
#' @param x A data.frame with missing values to impute
#' @import kknn
knnImpute <- function(x){
  stop('Not implemented')
  kknn::kknn(x, x)
}
