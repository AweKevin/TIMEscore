#' Plotting time-dependent ROC based on input TIMEscore and survival information
#'
#' @param score a data.frame which is the output of function 'TIMEscore'
#' @param survival survival information which is a data.frame rownamed with sample IDs identical to the rownames of output of function 'TIMEscore'. It is colnamed with 'status' and 'time' separately. Time unit has to be 'Year'.
#' @param times a numeric vector, time points for calculating AUC
#' @param color color vector of different time-dependent ROC
#' @param title main title of timeROC
#' @param width width of pdf file
#' @param height height of pdf file
#' @param filename output filename
#'
#' @return a pdf file of time-dependent ROC
#' @export
#' @import dplyr
#' @import stringr
#' @import tibble
#' @import timeROC
#' @importFrom survival Surv
#' @importFrom grDevices dev.off pdf
#' @importFrom graphics legend lines plot
#'
#' @examples
#'
#' data(exprSet)
#' exprSet[1:5, 1:5]
#' data(survival)
#' survival2 <- survival
#' survival2$time <- survival2$time / 365
#' head(survival2)
#' score <- TIMEscore(expr = exprSet)
#' library(survival)
#' TimeROC(score, survival2)
#'
TimeROC <- function(score, survival, times = c(1, 2, 3), color = c("slateblue", "seagreen3", "firebrick1"), title = "Time-dependent ROC", width = 4, height = 4, filename = "timeROC") {
  rt <- survival %>% tibble::rownames_to_column("sample") %>% dplyr::inner_join(score %>% rownames_to_column("sample"), by = "sample")
  ROC.DSST <- timeROC::timeROC(
    T = rt$time, delta = rt$status,
    marker = rt$TIMEscore, cause = 1,
    weighting = "marginal",
    times = times, ROC = TRUE,
  )

  pdf(str_c(filename, ".pdf"), 4, 4)
  plot(c(0, 1), c(0, 1), type = "l", xlab = "False positive rate", ylab = "Ture positive rate", main = title)
  for (i in 1:length(ROC.DSST$times)) {
    los <- lowess(ROC.DSST$FP[, i], y = ROC.DSST$TP[, i], f = 1 / 3, iter = 100)
    los$x <- c(0, los$x, 1)
    los$y <- c(0, los$y, 1)
    lines(los, col = color[i], lwd = 3)
  }
  lb <- "year(s)"
  legend("bottomright", paste0(
    ROC.DSST$times,
    "-", lb,
    " "
    ,' AUC='
    , round(ROC.DSST$AUC, 3)
  ), bty = "n", lwd = 3, col = color)
  dev.off()
}
