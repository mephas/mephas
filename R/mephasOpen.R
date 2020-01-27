##'
##' MEPHAS open function
##'
##' @title MEPHAS Open Windows
##'
##' @return shiny interface
##'
##' @examples
##' # mephas::mephasOpen("condist")
##' ## or,
##' # library(mephas)
##' # mephasOpen("condist")
##' @param method choose method from MEPHAS
##'
##' @export
mephasOpen <- function(method){
  switch(method,
         condist = MFScondist(),
         disdist = MFSdisdist(),

         ttest   = MFSttest(),
         nptest  = MFSnptest(),
         proptest= MFSproptest(),
         tabletest=MFSrctabtest(),
         anova   = MFSanova(),
         
         linereg = MFSlr(),
         logisreg= MFSlogit(),
         surv    = MFSsurv(),

         pca     = MFSpca(),
         pls     = MFSpls()
         )

}
