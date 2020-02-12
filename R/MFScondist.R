##'
##' MFScondist includes probability distribution of
##' (1) normal distribution
##' (2) exponential distribution
##' (3) gamma distribution
##' (4) beta distribution
##' (5) t distribution
##' (6) chi-square distribution
##' and (7) F distribution.
##'
##' MFScondist also generates random numbers draw the distribution of user data
##'
##' @title MEPHAS: Continuous Probability Distribution (Probability)
##'
##' @return shiny interface
##'
##' @importFrom stats dchisq dnorm dt pbinom pnorm ppois qchisq qexp qf qgamma qnorm qt quantile rchisq rexp rf rgamma rnorm rt sd var qbeta rbeta cor reshape
##' @importFrom utils read.csv write.csv head install.packages
##' @importFrom plotly plotlyOutput renderPlotly ggplotly layout plot_ly add_trace
##' @importFrom shinyWidgets switchInput 
##' @importFrom magrittr %>% 
##'
##' @examples
##' if (interactive()) {
##'  MFScondist()
##' }
##' 
##' if (interactive()) {
##'  mephasOpen("condist")
##' }


##' @export
MFScondist <- function(){

requireNamespace("mephas.tools", quietly = TRUE)
  suppressPackageStartupMessages(suppressMessages(
    shiny::runApp(system.file("1_1MFScondist", package = "mephas"))
  ))
}