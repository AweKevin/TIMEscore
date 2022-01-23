#' Calculating TIMEscore for every sample.
#'
#' @param expr a data.frame rownamed with gene symbols and colnamed with sample IDs
#' @return a data.frame containing sample IDs and corresponding TIMEscore
#' @export
#' @import dplyr
#' @examples
#'
#' data(exprSet)
#' exprSet[1:5, 1:5]
#' result <- TIMEscore(expr = exprSet)
#' head(result)
TIMEscore <- function(expr) {
  expr_p <- TIMEscore::exprSet[TIMEscore::PCA_df_p$gene, ]
  expr_n <- TIMEscore::exprSet[TIMEscore::PCA_df_n$gene, ]

  l_p <- lapply(names(expr_p), FUN = function(x) {TIMEscore::PCA_df_p$index %*% expr_p[ , x]})
  score_p <- do.call(c, l_p)

  l_n <- lapply(names(expr_n), FUN = function(x) {TIMEscore::PCA_df_n$index %*% expr_n[ , x]})
  score_n <- do.call(c, l_n)

  score <- score_p - score_n
  df <- score %>% as.data.frame()
  rownames(df) <- names(expr)
  names(df)[1] <- "TIMEscore"
  return(df)
}
