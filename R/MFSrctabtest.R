##'
##' MFSrctabtest includes test for counts data:
##' (1) chi-square test for 2x2, 2xC, RxC table,
##' (2) kappa test for 2xk, kxk table,
##' and (3) 2xC and RxC table under K confounding categories
##'
##' @title MEPHAS: Test for Contingency Table (Hypothesis Testing)
##'
##' @return The web-based GUI and interactive interfaces
##'
##' @importFrom psych cohen.kappa
##' @importFrom stats mantelhaen.test addmargins chisq.test fisher.test mcnemar.test
##'
##' @examples
##' if (interactive()) {
##'  MFSrctabtest()
##' }
##' 
##' if (interactive()) {
##'  mephasOpen("tabletest")
##' }

##' @export
MFSrctabtest <- function(){

#requireNamespace("mephas.tools", quietly = TRUE)
  suppressPackageStartupMessages(suppressMessages(
    shiny::runApp(system.file("5MFSrctabtest", package = "mephas"))
  ))
}