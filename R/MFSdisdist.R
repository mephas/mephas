##'
##' MFSdisdist includes probability distribution of
##' (1) binomial distribution
##' and (2) Poisson distribution
##'
##' MFScondist also generates random numbers draw the distribution of user data
##'
##' @title MEPHAS: Discrete Probability Distribution (Probability)
##'
##' @return shiny interface
##'
##' @importFrom stats pbinom ppois rbinom rpois
##'
##' @examples
##' if (interactive()) {
##'  MFSdisdist()
##' }
##' 
##' if (interactive()) {
##'  mephasOpen("disdist")
##' }
##'
##'
##' @export
MFSdisdist <- function(){

  suppressPackageStartupMessages(
    shiny::runApp(system.file("1_2MFSdisdist", package = "mephas"))
  )
}