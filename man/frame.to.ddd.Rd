% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/frame.to.ddd.R
\name{frame.to.ddd}
\alias{frame.to.ddd}
\title{Title}
\usage{
frame.to.ddd(
  frame,
  response.column,
  time.column,
  censor.column,
  case.weight.column,
  failure.mode.column,
  right.censor.names = GetSMRDDefault("SMRD.RcName"),
  left.censor.names = GetSMRDDefault("SMRD.LcName"),
  interval.censor.names = GetSMRDDefault("SMRD.IcName"),
  sinterval.censor.names = GetSMRDDefault("SMRD.DefaultSintervalCensorNames"),
  failure.censor.names = GetSMRDDefault("SMRD.FailName"),
  data.title = deparse(substitute(the.frame)),
  time.units = NULL,
  response.units = NULL,
  x.columns = NULL,
  xlabel = NULL,
  data.note = "",
  func.call = match.call(),
  file.name,
  skip = 0
)
}
\arguments{
\item{frame}{}

\item{response.column}{}

\item{time.column}{}

\item{censor.column}{}

\item{case.weight.column}{}

\item{failure.mode.column}{}

\item{right.censor.names}{}

\item{left.censor.names}{}

\item{interval.censor.names}{}

\item{sinterval.censor.names}{}

\item{failure.censor.names}{}

\item{data.title}{}

\item{time.units}{}

\item{response.units}{}

\item{x.columns}{}

\item{xlabel}{}

\item{data.note}{}

\item{func.call}{}

\item{file.name}{}

\item{skip}{}
}
\description{
Title
}
\examples{
\dontrun{

InsulationBrkdwn.ddd <- frame.to.ddd(insulationbrkdwn,
                                     response.column = 3, 
                                     time.column = 1,
                                     x.columns = 2,
                                     data.title = "Voltage Breakdown Data",
                                     response.units = "Volts",
                                     time.units = "Weeks")

print(InsulationBrkdwn.ddd)

plot(InsulationBrkdwn.ddd,
     transformation.Response = "log",
     transformation.time = "linear")

tmp <- groupi.Dest.Degrad.indivplots(InsulationBrkdwn.ddd,
                                     transformation.Response = "log", 
                                     transformation.time = "linear",
                                     distribution = "normal")

groupi.Dest.Degrad.oneplot(InsulationBrkdwn.ddd,
                           transformation.Response = "log", 
                           transformation.time = "linear",
                           distribution="normal")
groupm.Dest.Degrad(InsulationBrkdwn.ddd, 
                   distribution = "normal",
                   transformation.Response = "log10",
                   transformation.x = "invtemp",
                   transformation.time = "linear")


groupm.Dest.Degrad(InsulationBrkdwn.ddd, 
                   distribution = "normal",
                   transformation.Response = "log",
                   transformation.x = "arrhenius",
                   transformation.time="linear")

# Do individual analyses at each level of temperature

InsulationBrkdwn.groupi.Dest.Degrad <-groupi.Dest.Degrad(InsulationBrkdwn.ddd,
                                                         distribution = "normal",
                                                         transformation.Response = "log", 
                                                         transformation.time = "sqrt")


plot(InsulationBrkdwn.groupi.Dest.Degrad,
     transformation.x = "Arrhenius")

InsulationBrkdwn.groupm.Dest.Degrad <-groupm.Dest.Degrad(InsulationBrkdwn.ddd,
                                                         distribution = "normal", 
                                                         transformation.Response = "log",
                                                         transformation.x = "arrhenius", 
                                                         transformation.time = "sqrt")

InsulationBrkdwn.groupm.Dest.Degrad<-groupm.Dest.Degrad(InsulationBrkdwn.ddd,
                                                        distribution = "normal",
                                                        transformation.Response = "log",
                                                        transformation.x = "arrhenius",
                                                        transformation.time = "sqrt",
                                                        new.data = c("150,260"))


}
}
