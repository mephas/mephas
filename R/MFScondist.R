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
##' @import shiny
##' @import shinythemes
##' @import ggplot2
##'
##' @importFrom stats dchisq dnorm dt pbinom pnorm ppois qchisq qexp qf qgamma qnorm qt quantile rchisq rexp rf rgamma rnorm rt sd var qbeta rbeta cor reshape
##' @importFrom utils read.csv write.csv head
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

if (!requireNamespace("shiny")) {install.packages("shiny")}; requireNamespace("shiny")
if (!requireNamespace("ggplot2")) {install.packages("ggplot2")}; requireNamespace("ggplot2")
if (!requireNamespace("plotly")) {install.packages("plotly")}; requireNamespace("plotly")
if (!requireNamespace("shinyWidgets")) {install.packages("shinyWidgets")}; requireNamespace("shinyWidgets")

  suppressPackageStartupMessages(
    shiny::runApp(system.file("1_1MFScondist", package = "mephas"))
  )
}