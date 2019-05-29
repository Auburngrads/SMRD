#' @export
print.groupm.out <-
function (x, stress.state = NULL,...) 
{
  
  vcv <- as.matrix(x$groupm.out$vcv)
  parameter.correlation.matrix <- as.matrix(x$groupm.out$correl)
  theta<-matrix(x$groupm.out[['theta.hat']], nrow = 1)
  theta.names<-names(x$groupm.out[['parameter.fixed']])
  colnames(theta)<-theta.names
  ml.parameter.estimates <- theta
  
  zz<-list(vcv.matrix.total = vcv, 
           param.correl.matrix = parameter.correlation.matrix, 
           mle.param = ml.parameter.estimates)
  
  stresses <- x$groupm.out$stress.names
  stress.frame<-data.frame(stresses)
  colnames(stress.frame)<-""
  stress.mat<-matrix(stresses)
  colnames(stress.mat)<-"Stress State"
  
  if (is.null(stress.state)) {
  
    message("\nFor the overall dataset: \n")
    
    print(zz)
    
      
  cat(paste(c("This data set has",length(stresses), "unique stress states. \n"), collapse = " "))
  
  print(stress.frame)
  
  cat("\n")
  stress.state <- as.numeric(readline(prompt = "For which stress state would like print a summary?\nEntering 0 will return a summary of the full data\n"))
  
  }
  
  if (is.numeric(stress.state) && !stress.state==0) {
  
  return( print(x[[stress.mat[stress.state]]]) )
    
  }
  
  if (stress.state==0) { zz }
  
}