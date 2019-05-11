#' Flag if a character object is either NA, NULL, or has zero characters
#' 
#' @param object An one-dimensional character-class object
#' @return Returns \code{TRUE} If \code{is.na(object)}, \code{is.na(object)}, or \code{nchar(object) == 0}
#'         otherwise returns \code{FALSE} 
nnnc0 <- function(object){
  
  null = is.null(object)
  na = is.na(object)
  nc0 = nchar(object) == 0
  
  `if`(any(null,na,nc0), return(TRUE), return(FALSE))
  
}