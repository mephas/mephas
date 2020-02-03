##'
##' MFSproptest includes test for binomial proportion of
##' (1) one single proportion,
##' (2) two proportions from independent groups,
##' (3) more than two groups,
##' and (4) trend in more than two groups.
##'
##' @title MEPHAS: Test for Binomial Proportion (Hypothesis Testing)
##'
##' @return shiny interface
##'
##' @importFrom stats binom.test chisq.test fisher.test mcnemar.test prop.test reshape prop.trend.test
##'
##' @examples
##' if (interactive()) {
##'  MFSproptest()
##' }
##' 
##' if (interactive()) {
##'  mephasOpen("proptest")
##' }

##' @export
MFSproptest <- function(){


  suppressPackageStartupMessages(
    shiny::runApp(system.file("4MFSproptest", package = "mephas"))
  )
}