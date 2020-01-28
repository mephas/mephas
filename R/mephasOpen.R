##'
##' Open MEPHAS interfaces
##'
##' @title Open MEPHAS  Windows
##'
##' @return shiny interface of MEPHAS
##'
##' @examples
##' # mephas::mephasOpen("condist")
##' ## or,
##' # library(mephas)
##' # mephasOpen("condist")
##' # Please use Stop and Quit Button to close the interface
##'
##' @param method choose method from MEPHAS,
##' "condist" opens continuous distribution,
##' "disdist" opens discrete distribution,
##' "ttest" opens t test,
##' "nptest" opens non-parametric test,
##' "proptest" opens test methods for proportions,
##' "tabletest" opens test methods contingency table,
##' "anova"opens ANOVA,
##' "linereg" opens linear regression,
##' "logisreg" opens logistic regression,
##' "surv" opens survival models,
##' "pca" opens principal component analysis,
##' "pls" opens partial least square methods
##'
##' @export
mephasOpen <- function(method=c("condist","disdist","ttest","nptest","proptest","tabletest","anova","linereg","logisreg","surv","pca","pls")){

method <- match.arg(method)

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
