#' Compute the Response 
#'
#' @param data.d 
#'
#' @return A vector of responses
#' @export
Response <-
function (data.d)
{
    frame.type <- data.object.type(data.d)
    
switch(frame.type[1], 
       frame.centered = { 
         
          response.column <- attr(data.d, "response.column")
          if (is.character(data.d) && length(data.d == 1)) data.d <- get(envir = .frame0, inherits = TRUE, data.d)
          the.response <- as.matrix(data.d[, response.column])
          col.names <- dimnames(data.d)[[2]]
          names(col.names) <- col.names
          dimnames(the.response) <- list(NULL, col.names[response.column])
          
   }, list.centered = {
     
        response.column <- data.d$response.column
        the.response <- data.d$frame[, response.column]
        
    }, unfolded = {
      
        the.response <- data.d$y
        if (is.null(the.response)) the.response <- data.d$Response
        
    }, {
        stop("Corrupted data frame")
    })
    
if (is.null(the.response)) stop("Null response")
    
return(the.response)
}


`Response<-` <-
function (data.ld, value)
{
  value <- as.matrix(value)
  the.oldResponse <- Response(data.ld)
  
  if (ncol(value) == ncol(the.oldResponse)) {
    
  Response.names <- dimnames(the.oldResponse)[[2]]
  
  } else {
    
    if (ncol(value) == 2 && ncol(the.oldResponse) == 1) {
       Response.names <- paste(c(dimnames(the.oldResponse)[[2]],
                                 dimnames(the.oldResponse)[[2]]), 
                                 c("L", "U"),
                                 sep = "")
        } else {
          
       `if`(ncol(value) == 1 && ncol(the.oldResponse) ==  1,
            Response.names <- dimnames(the.oldResponse)[[2]][1],
            stop(paste("BadResponse new col =", 
                       ncol(value),
                       "and old col =", 
                        ncol(the.oldResponse))))
        }
        
        dimnames(value) <- list(as.character(1:nrow(value)),
                                Response.names)
      }
  
     Response.length <- nrow(value)
     
      if (Response.length != nrow(data.ld))
          stop(paste("InsertingResponse  with length",
                     Response.length,
                     "but data objects had", 
                     nrow(data.ld), 
                     "rows"))
     
      the.response.columns <- attr(data.ld, "response.column")
      all.attributes <- attributes(data.ld)
      frame.names <- names(data.ld)
      response.col.numbers <- match(names(data.ld[, the.response.columns,
                                                  drop = F]), frame.names)
      `if`(ncol(data.ld) > length(response.col.numbers),
           new.frame <- data.frame(data.ld[, -response.col.numbers, drop = F], 
                                   value),
           new.frame <- data.frame(value))
      
      all.attributes$response.column <- dimnames(value)[[2]]
      all.attributes$names <- names(new.frame)
      attributes(new.frame) <- all.attributes
      return(new.frame)
    }
