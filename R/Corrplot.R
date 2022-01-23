#' Plotting corrplot based on input TIMEscore and specified gene
#'
#' @param score a data.frame which is the output of function 'TIMEscore'
#' @param expr a data.frame rownamed with gene symbols and colnamed with sample IDs
#' @param gene gene symbol, for example, CD274
#' @param method method performing correlation analysis, one of 'pearson' and 'spearman'
#' @param color color of corrplot
#' @param width width of pdf file
#' @param height height of pdf file
#' @param filename output filename
#'
#' @return a pdf file of corrplot
#' @export
#' @import dplyr
#' @import stringr
#' @import tibble
#' @import ggplot2
#' @import ggpubr
#' @importFrom stats lm lowess
#'
#' @examples
#'
#' data(exprSet)
#' exprSet[1:5, 1:5]
#' score <- TIMEscore(expr = exprSet)
#' Corrplot(score, exprSet, "CD274")
#'
Corrplot <- function(score, expr, gene, method = "pearson", color = "#C45A44", width = 4.5, height = 4.5, filename = "corrplot") {
  tmp <- expr[gene, ] %>% t() %>% as.data.frame() %>% tibble::rownames_to_column("sample")
  df <- score %>% tibble::rownames_to_column("sample") %>%
    inner_join(tmp, by = "sample") %>%
    column_to_rownames("sample")
  p <- ggplot(data = df, aes(x = df[, 1], y = df[, 2])) +
    geom_point(color = color, size = 2) +
    geom_smooth(method = lm, lwd = 1.5, se = T, color = color) +
    labs(x = names(df)[1], y = names(df)[2]) +
    geom_rug(color = color)+
    ggpubr::theme_classic2() +
    theme(
      axis.title = element_text(size = 16),
      axis.text.y = element_text(size = 12, colour = "black"),
      axis.text.x = element_text(size = 12, colour = "black")
    ) +
    stat_cor(method = method, aes(x = df[, 1], y = df[, 2]), size = 5.5)
  ggsave(str_c(filename, ".pdf"), plot = p, width = width, height = height)
}
