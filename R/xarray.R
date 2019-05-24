#' Convert a LaTex-style array for use in rmarkdown
#' 
#' @description This function utilizes \code{xtable::xtable} to 
#'              return an array class object. This is useful for 
#'              rendering matrices in rmarkdown documents and
#'              presentations
#' @param table A \code{data.frame} or \code{matrix} to be converted
#' @param digits Sets the number of significant digits for numeric values
#' @param matrix A \code{logical} indicating if \code{table} is a 
#'               matrix-class object.
#' @param ... Additional options used in \code{xtable::print.xtable}
#' 
#' @return An printed LaTeX array class object 
#' @importFrom xtable xtable print.xtable 
#' @export 
xarray <- function(table, digits = NULL, matrix = FALSE,...) {

  if (!matrix) {
  table<- print(xtable::xtable(table, caption = NULL, digits = digits), comment = FALSE, print.results = FALSE,...)
  array1<-gsub("\\\\begin\\{table\\}\\[ht\\]\\\n\\\\centering\\\n\\\\begin\\{tabular\\}",
               "\\\\begin\\{array\\}", table  )
  array2<-gsub("end\\{tabular\\}\\\n\\\\end\\{table\\}","end\\{array\\}", array1)
  array3<-gsub("\\n"," ",array2)
  array4<-gsub("\\$","",array3)

  } else {
    
    table<- print(xtable::xtable(table, caption = NULL, digits = digits), comment = FALSE, 
                  print.results = FALSE, include.rownames = FALSE, include.colnames = FALSE,...)
    array1<-gsub("\\\\begin\\{table\\}\\[ht\\]\\\n\\\\centering\\\n\\\\begin\\{tabular\\}",
                 "\\\\begin\\{array\\}", table  )
    array2<-gsub("end\\{tabular\\}\\\n\\\\end\\{table\\}","end\\{array\\}", array1)
    array3<-gsub("\\n"," ",array2)
    array4<-gsub("\\$","",array3)
    array4<-gsub("\\\\hline","",array4)
    
  }
  
  return(array4)       }
