##'
##' MFSlr includes
##' (1) linear regression for continuous outcomes
##'
##' @title MEPHAS: Linear Regression Model (Advanced Method)
##'
##' @return shiny interface
##'
##'
##' @importFrom stats anova as.formula lm predict residuals step relevel
##' @importFrom utils str write.table
##'
##' @examples
##' if (interactive()) {
##'  MFSlr()
##' }
##' 
##' if (interactive()) {
##'  mephasOpen("linereg")
##' }
##'
##' @export
MFSlr <- function(){

requireNamespace("mephas.tools", quietly = TRUE)
  suppressPackageStartupMessages(suppressMessages(
    shiny::runApp(system.file("7_1MFSlr", package = "mephas"))
  ))
}