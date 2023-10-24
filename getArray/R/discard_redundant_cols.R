#' Get rid of all metadata colmuns that show duplicated information or are uninformative (only-1 or all-different variable values)
#'
#' @description
#' Leave only unique-information columns. Generally metadata includes the same variable information expressed in different ways through different columns. This functions detects an eliminates all of those.
#'
#' @return A dataframe object
#'
#' @examples
#'
#'@export
discard_redundant_cols = function(df){
  # Simplify all categories to numbers  (One-hot encoding)
  col_simplification = function(col){
    dicc = setNames(c(1:length(unique(col))), unique(col))
    return(sapply(col, function(x){unname(dicc[x])}))
  }
  # Duplicated cols, cols with all 1s and cols without var-groups (1,2,3...nrow) are removed
  final_df = as.data.frame(df[,!(duplicated(t(new_df)) | colSums(new_df)==nrow(new_df) | colSums(new_df)==sum(c(1:nrow(new_df))))])
  colnames(final_df) = colnames(df)[!(duplicated(t(new_df)) | colSums(new_df)==nrow(new_df) | colSums(new_df)==sum(c(1:nrow(new_df))))]
  return(final_df)
}
