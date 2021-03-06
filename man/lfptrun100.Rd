% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{lfptrun100}
\alias{lfptrun100}
\title{Integrated circuit life test (truncated)}
\format{
A \code{data.frame} with 28 rows and 34variables:
\tabular{rlll}{
  [, 1] \tab hours \tab Accumulated time at \code{event} \tab \bold{Numeric}\cr
  [, 2] \tab count \tab Event observed at \code{hours} (failure/right-censored) \tab \bold{Categoric}\cr
  [, 3] \tab truntime \tab Truncation time \tab \bold{Numeric}\cr
  [, 4] \tab truntype \tab Truncation type \code{hours} \tab \bold{Categoric}
  }
}
\source{
Meeker, W. Q. (1987), Limited failure population life tests: application to integrated circuit reliability, Technometrics, 29, 51-65.
}
\description{
The primary purpose of this experiment was to estimate the proportion of 
             defective units being manufactured in the current production process and 
             to estimate the amount of 'burn-in' time that would be required to remove 
             most of the defective units from the product population.  The engineers 
             involved in the experiment were also interested in whether it might be 
             possible to get the needed information about the state of the production process. 
             In the future, using much shorter tests (say 200 or 300 hours).
}
\details{
The \code{event} column indicates that these data are singly right censored at 1370 hours.
         However, the presence of ties indicates that the data are actually inspection times which 
         perhaps should have been recorded as interval censored observations.
}
\seealso{
\code{\link{lfp1370}}
}
