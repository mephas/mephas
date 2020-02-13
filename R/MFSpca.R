##'
##' MFSpca includes
##' (1) principal component analysis
##' and (2) factor analysis
##'
##' @title MEPHAS: Dimensional analysis 1 (Advanced Method)
##'
##' @return shiny interface
##'
##'
##' @importFrom stats biplot cor prcomp screeplot
##'
##' @examples
##' if (interactive()) {
##'  MFSpca()
##' }
##' 
##' if (interactive()) {
##'  mephasOpen("pca")
##' }
##'
##' @export
MFSpca <- function(){

#requireNamespace("mephas.tools", quietly = TRUE)
  suppressPackageStartupMessages(suppressMessages(
    shiny::runApp(system.file("8_1MFSpca", package = "mephas"))
  ))
}