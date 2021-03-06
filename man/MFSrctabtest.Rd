% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MFSrctabtest.R
\name{MFSrctabtest}
\alias{MFSrctabtest}
\title{MEPHAS: Test for Contingency Table (Hypothesis Testing)}
\usage{
MFSrctabtest()
}
\value{
The web-based GUI and interactive interfaces
}
\description{
MFSrctabtest includes test for counts data:
(1) chi-square test for 2x2, 2xC, RxC table,
(2) kappa test for 2xk, kxk table,
and (3) 2xC and RxC table under K confounding categories
}
\examples{
if (interactive()) {
 MFSrctabtest()
}

if (interactive()) {
 mephasOpen("tabletest")
}
}
