SMRDexamples <- function() {
  
  exp <- read.table(system.file("extdata", '1_examples.txt', package = "SMRD"), 
                    header = TRUE)

    exp<-exp[,-1]
  
    exp[,2] <- tolower(gsub('.txt','',exp[,2]))
  
    colnames(exp) <- c('Example','Data set')
  
    return(exp)
}