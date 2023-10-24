#' Get rid of all samples that have not enough biological replicates
#'
#' @description
#' Get rid of all samples that have not enough biological replicates
#'
#' @return A dataframe object
#'
#' @examples
#'
#'@export
discard_non_replicated_samples = function(df, min_replicates=3){
  # Set as NA those categories/values with less than the minium number of biological
  # replicates accepted by the user (min_replicates) in the dataframe given (df).
  usable_cols = c()
  for (n_col in 1:ncol(df)){
    del_values = names(table(df[,n_col])[table(df[,n_col])<min_replicates])
    df[,n_col][df[,n_col] %in% del_values] = NA
    # If any column is left with less than two comparing variables discards it
    if (length(table(df[,n_col]))>1){usable_cols = c(usable_cols, n_col)}
  }
  return(df[,usable_cols])
}
