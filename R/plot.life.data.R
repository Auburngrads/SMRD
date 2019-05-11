#' @export
plot.life.data <-
function (x, distribution = NULL, ...)
{
  
# if distribution is NA, NULL, or nchar(distribution) == 0
  if(missing(distribution) | nnnc0(distribution)) { 
      
     tmp <- cdfplot(data.ld = x, ...) 
     return(invisible(tmp))
       
  }
  
# If a single distribution is given
  if(length(distribution) == 1) {
      
     tmp <- npprobplot(data.ld = x, distribution = distribution,...)
     return(invisible(tmp))
        
  }
    
# If a vector of distributions are given
  if(length(distribution) > 1) {
    
     rows = floor(length(distribution) / 2) +  length(distribution) %% 2

     matc = integer(rows * 2)

     for(i in 1:length(distribution)) matc[i] = matc[i] + i
     
     layout(mat = matrix(matc, nrow = rows, ncol = cols, byrow = T))

     for(i in 1:length(distribution)){
       
        tmp <- npprobplot(data.ld = data.ld, 
                          distribution = distribution[i],
                          my.title = paste0(toupper(distribution[i])),...)
       
     }      
  
     return(invisible(tmp))
     
  }
     
}
