##'
##' MFSpls includes
##' (1) principal component regression
##' (2) partial least squares regression
##' and (3) sparse partial least squares regression
##'
##' @title MEPHAS: Dimensional analysis 2 (Advanced Method)
##'
##' @return The web-based GUI and interactive interfaces
##'
##'
##' @importFrom pls mvr R2 MSEP RMSEP
##' @importFrom spls cv.spls spls
##' @importFrom graphics plot
##'
##' @examples
##' if (interactive()) {
##'  MFSpls()
##' }
##' 
##' if (interactive()) {
##'  mephasOpen("pls")
##' }
##'
##' @export
MFSpls <- function(){
requireNamespace("mephas.tools", quietly = TRUE)
  suppressPackageStartupMessages(suppressMessages(
    shiny::runApp(system.file("8_2MFSpls", package = "mephas"))
  ))
}