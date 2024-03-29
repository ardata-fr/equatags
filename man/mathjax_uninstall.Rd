% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mathjax_node_setup.R
\name{mathjax_uninstall}
\alias{mathjax_uninstall}
\title{Uninstall 'mathjax-node'}
\usage{
mathjax_uninstall()
}
\value{
a single logical value, FALSE if the operation
failed, TRUE otherwise.
}
\description{
Removes 'mathjax-node'.

'mathjax-node' should be removed with
command \code{\link[=mathjax_uninstall]{mathjax_uninstall()}} because it is not used
anymore thanks to package 'katex'.
}
\examples{
if(mathjax_available()) {
  mathjax_uninstall()
}
}
\seealso{
Other tools for 'mathjax-node': 
\code{\link{mathjax_available}()}

Other tools for 'mathjax-node': 
\code{\link{mathjax_available}()}
}
\concept{tools for 'mathjax-node'}
