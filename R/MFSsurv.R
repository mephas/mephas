
##'
##' MFSsurv includes
##' (1) K-M estimation and log-rank test for survival probability curves,
##' (2) Cox regression
##' and (3) accelerated failure time model
##'
##' @title MEPHAS: Survival Analysis (Advanced Method)
##'
##' @return shiny interface
##'
##' @import survival
##' @import survminer
##'
##' @importFrom survAUC predErr AUC.cd AUC.sh AUC.uno AUC.hc
##' @importFrom stats resid
##'
##' @examples
##' if (interactive()) {
##'  MFSsurv()
##' }
##' 
##' if (interactive()) {
##'  mephasOpen("surv")
##' }
##'
##' @export
MFSsurv <- function(){

requireNamespace("mephas.tools", quietly = TRUE)
  suppressPackageStartupMessages(suppressMessages(
    shiny::runApp(system.file("7_3MFSsurv", package = "mephas"))
  ))
}