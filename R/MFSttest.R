
##'
##' MFSttest includes t test of
##' (1) one sample,
##' (2) two independent samples,
##' and (3) two paired samples.
##'
##' @title MEPHAS: T Test (Hypothesis Testing)
##'
##' @return shiny interface
##'
##' @importFrom reshape melt
##' @importFrom stats t.test var.test
##' @examples
##' if (interactive()) {
##'  MFSttest()
##' }
##' 
##' if (interactive()) {
##'  mephasOpen("ttest")
##' }

##' @export
MFSttest <- function(){

#requireNamespace("mephas.tools", quietly = TRUE)
  suppressPackageStartupMessages(suppressMessages(
    shiny::runApp(system.file("2MFSttest", package = "mephas"))
  ))
}