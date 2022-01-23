#' Plotting survival curve based on input TIMEscore and survival information
#'
#' @param score a data.frame which is the output of function 'TIMEscore'
#' @param survival survival information which is a data.frame rownamed with sample IDs identical to the rownames of output of function 'TIMEscore'. It is colnamed with 'status' and 'time' separately.
#' @param unit time unit of time column in param survival, which is one of ’Days‘, 'Months', 'Years'.
#' @param color color vector of high- and low-group
#' @param linesize a number referring to the line size of high- and low-group
#' @param width width of pdf file
#' @param height height of pdf file
#' @param filename output filename
#'
#' @return a pdf file of survival curve
#' @export
#' @import dplyr
#' @import stringr
#' @import tibble
#' @import ggplot2
#' @import survival
#' @import survminer
#' @importFrom grDevices dev.off pdf
#' @importFrom stats median
#' @examples
#'
#' data(exprSet)
#' exprSet[1:5, 1:5]
#' data(survival)
#' head(survival)
#' score <- TIMEscore(expr = exprSet)
#' Survplot(score, survival)
#'
Survplot <- function(score, survival, unit = "Days", color = c("#cf273c", "#5091c0"), linesize = 1, width = 4.5, height = 5.5, filename = "survplot") {
  surv_df <- survival %>% tibble::rownames_to_column("sample") %>% dplyr::inner_join(score %>% rownames_to_column("sample"), by = "sample")
  surv_df <- surv_df %>% dplyr::mutate(group = ifelse(TIMEscore >= median(TIMEscore), "high", "low"))
  sfit <- survival::survfit(survival::Surv(time, status) ~ group, data = surv_df)
  diff <- survival::survdiff(formula = survival::Surv(time, status) ~ group, data = surv_df, rho = 0)
  pval <- stats::pchisq(diff$chisq, length(diff$n) - 1, lower.tail = FALSE)

  gg <- survminer::ggsurvplot(sfit, data = surv_df,
                   size = linesize,
                   risk.table = TRUE,
                   conf.int = F, # 置信区间
                   risk.table.y.text = F,
                   palette = color,
                   pval.method = F,
                   pval = ifelse(pval < 0.001, "p < 0.001", paste0("p = ", pval %>% round(digits = 3))),
                   ncensor.plot = F,
                   censor = F,
                   xlab = unit,
                   ylab = "Overall Survival",
                   legend = "top",
                   legend.title = "",
                   legend.labs = c("High-TIMEscore", "Low-TIMEscore")
  )
  pdf(str_c(filename, ".pdf"), width = width, height = height)
  print(gg)
  dev.off()
}
