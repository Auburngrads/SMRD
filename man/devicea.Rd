% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{devicea}
\alias{devicea}
\title{Temperature-accelerated life test data}
\format{
A \code{data.frame} with 37 rows and 4 variables:
\tabular{rlll}{
  [, 1] \tab hours   \tab Accumulated time at \code{event} \tab \bold{Numeric}\cr
  [, 2] \tab event   \tab Event observed at \code{hours} (failure/right-censored) \tab \bold{Categoric}\cr
  [, 3] \tab count   \tab Number of events observed at \code{hours} \tab \bold{Numeric}\cr
  [, 4] \tab celsius \tab Temperature applied to the device during testing \tab \bold{Numeric}
   }
}
\source{
Hoopers J. H. and Amster, S. J. (1990) 
        Analysis and presentation of reliability data, in Handbook of Statistical Methods for Engineers and Scientists, 
        McGraw-Hill, New York. Harrison M. Wadsworth, Editor.

Meeker W.Q. and Escobar L.A. (1998) Statistical Methods for Reliability Data, New York: John Wiley & Sons.
}
\description{
Hooper and Amster (1990) analyze the temperature-accelerated life test data on an unidentified device. Meeker
             and Escobar (1998) refer to this device as "Device A". The purpose of the experiment was to determine if "Device A" 
             would meet a failure rate objective through 10,000 hours and 30,000 hours at an operating ambient temperature of 
             10 degrees celsius.  Device samples were tested for up to 5000 hours at four separate temperatures.
}
