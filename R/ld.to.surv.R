ld.to.surv<-function(ld) {

  for (i in 1:ncol(ld)) {

    if(match(names(ld)[i],attr(ld,"response.column")[1],nomatch=0)) {begin <-ld[,i]}
    if(match(names(ld)[i],attr(ld,"response.column")[2],nomatch=0)) {end   <-ld[,i]}
    if(match(names(ld)[i],attr(ld,"censor.column"  )   ,nomatch=0)) {status<-ld[,i]}
    if(match(names(ld)[i],attr(ld,"case.weight.column"),nomatch=0)) {weight<-ld[,i]}

    if( is.null(attr(ld,"response.column")[2]) ||  is.na(attr(ld,"response.column")[2])) { end    <- begin }
    if( is.null(attr(ld,"censor.column")     ) ||  is.na(attr(ld,"censor.column")     )) { status <-rep("failed",length(begin)) }
    if( is.null(attr(ld,"case.weight.column")) ||  is.na(attr(ld,"case.weight.column"))) { weight <-rep(1,length(begin)) }
  }

  Time1<-c(rep(begin,weight))
  Time2<-c(rep(end  ,weight))
  Cen  <-c(tolower(as.character(rep(status,weight))))

  for (i in 1:length(Cen))  {
    Cen[i]<-ifelse(sum(Cen[i]==c(unlist(strsplit(SMRDOptionsDefaults()$SMRD.Rc,"[,]"))))==1,0,Cen[i])
    Cen[i]<-ifelse(sum(Cen[i]==c(unlist(strsplit(SMRDOptionsDefaults()$SMRD.Lc,"[,]"))))==1,2,Cen[i])
    Cen[i]<-ifelse(sum(Cen[i]==c(unlist(strsplit(SMRDOptionsDefaults()$SMRD.Ic,"[,]"))))==1,3,Cen[i])
    Cen[i]<-ifelse(sum(Cen[i]==c(unlist(strsplit(SMRDOptionsDefaults()$SMRD.Fa,"[,]"))))==1,1,Cen[i])  }

  #  Create the "Surv" object for R package {Survival}
  SURV<-survival::Surv(Time1,Time2,as.numeric(Cen),type="interval")

  return(SURV)

}
