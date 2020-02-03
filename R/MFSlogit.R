
##'
##' MFSlogit includes
##' (1) logistic regression for binary outcomes,
##'
##' @title MEPHAS: Logistic Regression Model (Advanced Method)
##'
##' @return shiny interface
##'
##'
##' @importFrom ROCR performance prediction
##' @importFrom stats anova as.formula binomial glm predict residuals step relevel
##'
##' @examples
##' if (interactive()) {
##'  MFSlogit()
##' }
##' 
##' if (interactive()) {
##'  mephasOpen("logisreg")
##' }
##'
##' @export
MFSlogit <- function(){

  suppressPackageStartupMessages(
    shiny::runApp(system.file("7_2MFSlogit", package = "mephas"))
  )
}