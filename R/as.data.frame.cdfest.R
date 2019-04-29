as.data.frame.cdfest <-
function (x, row.names = NULL, optional = FALSE,...)
{

  Begin<-x[[2]]
  if ( Begin[1] < 0 ) Begin[1]=0
  End<-x[[3]]
  if ( End[length(End)] > 1e10 ) End[length(End)]=Inf
  Fhat<-x[[4]]
  se<-x[[5]]
  w<-exp(qnorm(0.975)*se/(Fhat*(1-Fhat)))
  lower.ci<-Fhat/(Fhat+(1-Fhat)*w)
  upper.ci<-Fhat/(Fhat+(1-Fhat)/w)

  rframe<-data.frame(Begin, End, Fhat, se, lower.ci, upper.ci)

  if(is.nan(rframe$lower.ci[1])) {
    rframe$lower.ci[1]=0
  }

  if(is.nan(rframe$lower.ci[length(lower.ci)])) {
    rframe$lower.ci[length(lower.ci)]=1
  }

  if(is.nan(rframe$upper.ci[1])) {
    rframe$upper.ci[1]=0
  }

  if(is.nan(rframe$upper.ci[length(upper.ci)])) {
    rframe$upper.ci[length(upper.ci)]=1
  }

  return(rframe)

}
