% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FillRegion.R
\name{FillRegion}
\alias{FillRegion}
\title{Title}
\usage{
FillRegion(
  gmle.out,
  iter = 2000,
  conf.level = (GetSMRDDefault("SMRD.ConfLevel") + 1)/100,
  prt = T,
  lower = NULL,
  upper = NULL,
  cull = 1,
  nbound = 5,
  simult = F,
  debug1 = F
)
}
\arguments{
\item{gmle.out}{}

\item{iter}{}

\item{conf.level}{}

\item{prt}{}

\item{lower}{}

\item{upper}{}

\item{cull}{}

\item{nbound}{}

\item{simult}{}

\item{debug1}{}
}
\description{
Title
}
\examples{
\dontrun{

Fan.weibull.gmle.out <- ls.mle(Fan.ld,
                               distribution = "Weibull")

Fan.lev.gmle.out <- ls.mle(Fan.ld,
                           distribution = "lev")
names(Fan.weibull.gmle.out)

 Fan.weibull.gmle.out <- FillRegion(Fan.weibull.gmle.out,
                                    nbound = 4,
                                    iter = 500, 
                                    cull = 2)

summary(Fan.weibull.gmle.out.jcr)

mleprobplot(Fan.ld, 
            distribution = "Lognormal")

basic.gmleprobplot(Fan.ld,
                   distribution = "Weibull",
                   plot.dist = "Weibull",
                   xxx.mle.out = Fan.weibull.gmle.out,
                   my.title = "",
                   cexlab = 1.5,
                   conlev = 0.95,
                   ciMethod = "lr.approx",
                   length.time.vec = 20)

Fan.lognormal.gmle.out <- ls.mle(Fan.ld,
                                 distribution = "Lognormal")

basic.gmleprobplot(Fan.ld,
                   distribution = "Lognormal", 
                   plot.dist = "Lognormal",
                   xxx.mle.out = Fan.lognormal.gmle.out,
                   my.title ="",
                   cexlab = 1.5,
                   conlev = 0.95,
                   ciMethod = "lr.approx",
                   length.time.vec = 20)


BearingCage.weibull.gmle.out <- ls.mle(BearingCage.ld,
                                       distribution = "Weibull")

names(BearingCage.weibull.gmle.out)

BearingCage.weibull.gmle.out.jcr <-  FillRegion(BearingCage.weibull.gmle.out,
                                                nbound = 4,
                                                iter = 500,
                                                cull = 2)

summary(BearingCage.weibull.gmle.out.jcr)

mleprobplot(BearingCage.ld,
            distribution = "Weibull",
            xlim = c(200,10000),
            ylim = c(.00031,.19),
            time.vec = log.seq(200,10000,20))

basic.gmleprobplot(BearingCage.ld,
                   distribution = "Weibull",
                   xxx.mle.out = BearingCage.weibull.gmle.out,
                   my.title = "",
                   cexlab = 1.5,
                   conf.level = 0.95,
                   length.time.vec = 20,
                   xlim = c(200,10000),
                   ylim = c(.00031,.19), 
                   time.vec = log.seq(200,10000,20),
                   ciMethod = "lr.approx")

}
}
