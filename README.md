
# TIMEscore

<!-- badges: start -->
<!-- badges: end -->

The goal of TIMEscore is calculating TIMEscore and performing visualization

## Installation

You can install the development version of TIMEscore like so:

``` r
devtools::install_github("AweKevin/TIMEscore")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(TIMEscore)
data(exprSet)
score <- TIMEscore(expr = exprSet)
data(survival)
Survplot(score, survival)
survival2 <- survival
survival2$time <- survival2$time / 365
library(survival)
TimeROC(score, survival2)
Corrplot(score, exprSet, "CD274")
## basic example code
```

