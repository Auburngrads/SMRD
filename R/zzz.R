.onLoad <- function(libname, pkgname) {

  .frame0 <<- new.env() 

  .DATA <<- function(data.name, input.type = 'txt', output.type = 'rda', skip = 0,...) {

    #file.name <- paste(c(data.name,input.type), collapse = '.')

    extdata <- paste(c(gsub('.txt','',data.name),input.type),collapse = '.')
    
    path <- paste(c(getwd(),"/inst/extdata/",extdata), collapse = '')
    
    dat.name <- read.table(path, header = TRUE, skip = skip)
    
    # Do needed edits, then
    # write.table(dat.name, file = path, row.names = F, col.names = noquote(colnames(dat.name)))
    
    colnames(dat.name) <- tolower(colnames(dat.name))

    save.name <- tolower(gsub('.txt','',data.name))

    assign(save.name, dat.name, envir = environment())

    save(list = save.name, 
         file = paste(c("data/",tolower(gsub(input.type,output.type, data.name))), collapse = ''),
         compress = 'xz',
         compression_level = 9)

}
}

buildData <- function(name = NULL,Rd = TRUE,...) {
  
  path <- paste(c(getwd(),"/inst/extdata/"), collapse = '')
  
  bunch <- list.files(path)
  
  if( is.null(name)) { 
    
    for(i in 2:length(bunch)) {.data(bunch[i]) }
    
  } else {
    
    .data(bunch[which(tolower(bunch)%in%paste(c(name,'txt'), collapse ='.'))],...) 
  }
  
  if(Rd) devtools::document()
  
}

info <- function(info,...) {

  INFO <- switch(as.character(info),
                 'authors' = "W. Q. Meeker, L. A. Escobar, and J. K. Freels",
                 'book' = 'Statistical Methods for Reliability Data',
                 'edition' = '1st ed.',
                 'work' = "Air Force Institute of Technology",
                 'job'  = 'Assistant Professor of Systems Engineering',
                 'dept' = 'Department of Systems Engineering and Management',
                 'chapter1'  = 'Chapter 1 - Reliability Concepts and Reliability Data',
                 'chapter2'  = 'Chapter 2 - Models, Censoring, and Likelihood for Failure-Time Data',
                 'chapter3'  = 'Chapter 3 - Nonparametric Estimation',
                 'chapter4'  = "Chapter 4 - Location-Scale-Based Parametric Distributions",
                 'chapter5'  = 'Chapter 5 - Other Parametric Distributions',
                 'chapter6'  = 'Chapter 6 - Probability Plotting',
                 'chapter7'  = 'Chapter 7 - Parametric Likelihood Fitting Concepts: Exponential Distribution',
                 'chapter8'  = 'Chapter 8 - Maximum Likelihood for Log-Location-Scale Distributions',
                 'chapter9'  = 'Chapter 9 - Bootstrap Confidence Intervals',
                 'chapter10' = 'Chapter 10 - Planning Life Tests',
                 'chapter11' = 'Chapter 11 - Parametric Maximum Likelihood: Other Models',
                 'chapter12' = 'Chapter 12 - Prediction of Future Random Quantities',
                 'chapter13' = 'Chapter 13 - Degradation Data, Models and Data Analysis',
                 'chapter14' = 'Chapter 14 - Introduction to the Use of Bayesian Methods for Reliability Data',
                 'chapter15' = 'Chapter 15 - System Reliability Concepts and Methods',
                 'chapter16' = 'Chapter 16 - Analysis of Repairable System and Other Recurrence Data',
                 'chapter17' = 'Chapter 17 - Failure-Time Regression Analysis',
                 'chapter18' = 'Chapter 18 - Accelerated Test Models',
                 'chapter19' = 'Chapter 19 - Accelerated Life Tests',
                 'chapter20' = 'Chapter 20 - Planning Accelerated Life Tests',
                 'chapter21' = 'Chapter 21 - Accelerated Degradation Tests',
                 'chapter22' = 'Chapter 22 - Case Studies and Further Applications',
                 'chapter23' = 'Chapter 23 - Analysis of Accelerated Destructive Degradation Test (ADDT) Data',
                 'chapter24' = 'Chapter 24 - Accelerated Destructive Degradation Test (ADDT) Planning',
                 'chap1'  = 'Reliability Concepts and Reliability Data',
                 'chap2'  = 'Models, Censoring, and Likelihood for Failure-Time Data',
                 'chap3'  = 'Nonparametric Estimation',
                 'chap4'  = "Location-Scale-Based Parametric Distributions",
                 'chap5'  = 'Other Parametric Distributions',
                 'chap6'  = 'Probability Plotting',
                 'chap7'  = 'Parametric Likelihood Fitting Concepts: Exponential Distribution',
                 'chap8'  = 'Maximum Likelihood for Log-Location-Scale Distributions',
                 'chap9'  = 'Bootstrap Confidence Intervals',
                 'chap10' = 'Planning Life Tests',
                 'chap11' = 'Parametric Maximum Likelihood: Other Models',
                 'chap12' = 'Prediction of Future Random Quantities',
                 'chap13' = 'Degradation Data, Models and Data Analysis',
                 'chap14' = 'Introduction to the Use of Bayesian Methods for Reliability Data',
                 'chap15' = 'System Reliability Concepts and Methods',
                 'chap16' = 'Analysis of Repairable System and Other Recurrence Data',
                 'chap17' = 'Failure-Time Regression Analysis',
                 'chap18' = 'Accelerated Test Models',
                 'chap19' = 'Accelerated Life Tests',
                 'chap20' = 'Planning Accelerated Life Tests',
                 'chap21' = 'Accelerated Degradation Tests',
                 'chap22' = 'Case Studies and Further Applications',
                 'chap23' = 'Analysis of Accelerated Destructive Degradation Test (ADDT) Data',
                 'chap24' = 'Accelerated Destructive Degradation Test (ADDT) Planning',
                 'appendixb' = 'Appendix B - Review of Results from Statistical Theory')

  return(INFO)
}

vinny <- function(fw = 6, fh = 4,...)  {
  
  vign <-  function() {
    
    knitr::opts_chunk$set(message = FALSE,
                          warning = FALSE,
                          fig.align = 'center',
                          fig.width = fw,
                          fig.height = fh,
                          comment = NA,...)
    }
  vign()
}

.sysdata <<- function(data.name, input.type = 'txt', output.type = 'rda', skip = 0,...) {
  
  #file.name <- paste(c(data.name,input.type), collapse = '.')
  
  extdata <-paste(c(gsub('.txt','',data.name),input.type),collapse = '.')
  
  path <- paste(c(getwd(),"/inst/extdata/",extdata), collapse = '')
  
  dat.name <- read.table(path, header = TRUE, skip = skip)
  
  # Do needed edits, then
  # write.table(dat.name, file = path, row.names = F, col.names = noquote(colnames(dat.name)))
  
  colnames(dat.name) <- tolower(colnames(dat.name))
  
  save.name <- tolower(gsub('.txt','',data.name))
  
  assign(save.name, dat.name, envir = .GlobalEnv)
  
}

buildSysdata <- function(name = NULL,Rd = FALSE,...) {
  
  path <- paste(c(getwd(),"/inst/extdata/"), collapse = '')
  
  bunch <- list.files(path)
  
  if( is.null(name)) { 
    
    for(i in 2:length(bunch)) {.sysdata(bunch[i]) }
    
  } else {
    
    .data(bunch[which(tolower(bunch)%in%paste(c(name,'txt'), collapse ='.'))],...) 
  }
  
  if(Rd) devtools::document()
  
}

# for(i in 1:length(ls())) {
#   
#   datas[[i]] <- get(ls()[i], envir = .GlobalEnv)
# }
# 
# names(datas) <- ls()
  




