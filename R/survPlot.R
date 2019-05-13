#' @importFrom survival survfit Surv
survPlot<- function (ld) {

  for (i in 1:ncol(ld)) {

    if(match(names(ld)[i],attr(ld,"response.column")[1],nomatch=0)) {begin<-ld[,i]}
    if(match(names(ld)[i],attr(ld,"response.column")[2],nomatch=0)) {end<-ld[,i]}
    if(match(names(ld)[i],attr(ld,"censor.column"  )   ,nomatch=0)) {status<-ld[,i]}
    if(match(names(ld)[i],attr(ld,"case.weight.column"),nomatch=0)) {weight<-ld[,i]}

    if( is.null(attr(ld,"response.column")[2]) ||  is.na(attr(ld,"response.column")[2])) { end <- begin }
    if( is.null(attr(ld,"censor.column")     ) ||  is.na(attr(ld,"censor.column")     )) { status <-rep("failed",length(begin)) }
    if( is.null(attr(ld,"case.weight.column")) ||  is.na(attr(ld,"case.weight.column"))) { weight <-rep(1,length(begin)) }
  }

  Time1<-c(rep(begin,weight))
  Time2<-c(rep(end  ,weight))
  Cen<-c(tolower(as.character(rep(status,weight))))

  for (i in 1:length(Cen))  {
    Cen[i]<-ifelse(sum(Cen[i]==c(unlist(strsplit(SMRDOptionsDefaults()$SMRD.Rc,"[,]"))))==1,0,Cen[i])
    Cen[i]<-ifelse(sum(Cen[i]==c(unlist(strsplit(SMRDOptionsDefaults()$SMRD.Lc,"[,]"))))== 1,2,Cen[i])
    Cen[i]<-ifelse(sum(Cen[i]==c(unlist(strsplit(SMRDOptionsDefaults()$SMRD.Ic,"[,]")))) == 1,3,Cen[i])
    Cen[i]<-ifelse(sum(Cen[i]==c(unlist(strsplit(SMRDOptionsDefaults()$SMRD.Fa,"[,]"))))== 1,1,Cen[i])  }

  #  Create the "Surv" object for R package {Survival}
  SURV<-survival::Surv(Time1,Time2,as.numeric(Cen),type="interval")

  par(family="serif",cex=1.25)
  KM<-survival::survfit(SURV~1)
  plot(KM, ylim=range(min(KM$lower),1), col=c(1,4,4), lwd=c(2.,2,2), axes=FALSE, xlab="",ylab="")
  box(lwd=1.25)
  axis(side = 1, tck = -.015, labels = NA)
  axis(side = 2, tck = -.015, labels = NA)
  axis(side = 1, lwd = 0, line = -.6)
  axis(side = 2, lwd = 0, line = -.6, las = 1)
  mtext(side = 1, attr(ld,"time.units"), line = 1.75,cex=1.25)
  mtext(side = 2, "Fraction Surviving", line = 2.75,cex=1.25)

}

