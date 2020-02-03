
##'
##' MFSanova includes
##' (1) one-way ANOVA,
##' (2) pairwise post-hoc test for one-way ANOVA,
##' (3) two-way ANOVA,
##' (4) pairwise post-hoc test for two-way ANOVA
##' (5) Kruskal-Wallis test
##' and (6) post-hoc test for Kruskal-Wallis test
##'
##'
##' @title MEPHAS: ANOVA (Hypothesis Testing)
##'
##' @return shiny interface
##'
##' @importFrom psych describeBy 
##' @importFrom stats TukeyHSD aov pairwise.t.test anova kruskal.test relevel
##'
##' @examples
##' if (interactive()) {
##'  MFSanova()
##' }
##' 
##' if (interactive()) {
##'  mephasOpen("anova")
##' }


##' @export
MFSanova <- function(){

  suppressPackageStartupMessages(
    shiny::runApp(system.file("6MFSanova", package = "mephas"))
  )
}