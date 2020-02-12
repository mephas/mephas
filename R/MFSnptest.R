
##'
##' MFSnptest includes Non-parametric test of
##' (1) one sample,
##' (2) two independent samples,
##' and (3) two paried samples.
##'
##' @title MEPHAS: Non-parametric Test (Hypothesis Testing)
##'
##' @return shiny interface
##'
##' @importFrom stats wilcox.test
##'
##' @examples
##' if (interactive()) {
##'  MFSnptest()
##' }
##' 
##' if (interactive()) {
##'  mephasOpen("nptest")
##' }
##'
##'
##' @export
MFSnptest <- function(){

requireNamespace("mephas.tools", quietly = TRUE)
  suppressPackageStartupMessages(suppressMessages(
    shiny::runApp(system.file("3MFSnptest", package = "mephas"))
  ))
}