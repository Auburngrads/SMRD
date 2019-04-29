get.x.columns <-
function (data.d, allow = T)
{
    x.columns <- colnames(xmat(data.d, allow = allow))
    
    if (is.null(x.columns)) {
      
      if (allow) test.x.columns<-NULL  else stop("Null x.columns")
      
    }
    
    if (length(x.columns)==1) {
      
      x.columns <- attr(data.d, "x.columns")
      data.d <- as.data.frame(data.d)
      colnames(data.d) <- strsplit(colnames(data.d),",")
      
      test.x.columns <- data.d[c(which(colnames(data.d)%in%x.columns))]
      
    }
    
    if (length(x.columns) >= 2) {
      
      x.columns <- strsplit(attr(attr(data.d, "x.columns"), "names"),",")
      data.d<-as.data.frame(data.d)
      colnames(data.d)<-strsplit(colnames(data.d),",")
      test.x.columns <- data.d[c(which(colnames(data.d)%in%x.columns))]
      
    }

    if (!is.null(test.x.columns) && !all(x.columns%in%names(test.x.columns)))
        warning("colnames(xmat(data.d)) != attr(data.d, \"x.columns\")")
    return(test.x.columns)
}
