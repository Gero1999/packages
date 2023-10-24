#' Simplify labels of your categorical variables
#'
#' @description
#' Get rid of all unnecessarily complexities in your dataframe labels in order to simplify the language
#'
#' @return A dataframe object
#'
#' @examples
#'
#'@export
reduce_label_language = function(df){
  for (n_col in 1:ncol(df)){
    for (preposition in c('with', 'and', 'at', 'for')){
      df[,n_col]=gsub(preposition, '', df[,n_col])}
    for (sub_n in 1:10){
      df[,n_col]=gsub(c('deci', 'centi', 'milli', 'micro', 'nano', 'pico', 'liter', 'molar', 'mole', 'meter')[sub_n],
                      c('d', 'c', 'm', 'μ', 'n', 'p', 'L', 'M', 'M', 'm')[sub_n],
                      x = df[,n_col])}
    df[,n_col]=gsub(' ', '_', df[,n_col])
  }
  return(df)
}
