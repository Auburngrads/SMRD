# nocov start

.onLoad <- function(libname, pkgname) {

  .frame0 <<- new.env() 

}

.onUnload <- function (libpath) {
  library.dynam.unload("SMRD", libpath)
}

.onAttach = function(libname, pkgname) {
  # Runs when attached to search() path such as by library() or require()
  if (!interactive()) return()
  v = packageVersion("SMRD")
  br = read.dcf(system.file("DESCRIPTION", package="SMRD"), fields = c("BugReports"))
 
    packageStartupMessage("SMRD (version ", v, ") is experimental software under active development\n\n",
                          "If you encounter any errors or unexpected problems\n",
                          "please submit an issue at: ", br[1L],
                          "\n\nThe best way to start using SMRD is check out the echapters",
                          "\n\nFor example: echapter(chapter = 1)")
    
}


info <- function(info,...) {

  INFO <- switch(as.character(info),
                 'authors' = "W. Q. Meeker and L. A. Escobar",
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

vinny <- function(fw = 8, fh = 6,...)  {
  
  vign <- function() {
    
          knitr::opts_chunk$set(message = FALSE,
                                warning = FALSE,
                                fig.align = 'center',
                                fig.width = fw,
                                fig.height = fh,
                                comment = NA,...)
  }
  
  vign()
  
}

# nocov end