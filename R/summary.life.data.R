#' @export
summary.life.data <-
function (object, 
          printem = T, 
          print.limit = 50,...)
{
  
lda.type <- data.object.type(data.ld = object)

if(lda.type != "frame.centered") cat("\nOld-style life data object; consider rebuilding it\n")
data.ld <- SMRD.sanity.life.data(x = object)
the.response <- Response(data.ld)
number.cases <- nrow(the.response)
the.case.weights <- case.weights(data.ld)

if(is.null(the.case.weights)) the.case.weights <- rep(1, number.cases)

the.censor.codes <- censor.codes(data.ld)
if(is.null(the.censor.codes)) {
   
   the.censor.codes <- rep(1, number.cases)
   no.censoring <- T
   
 } else {
   
   no.censoring <- F
   
}

not.dummy <- the.case.weights > 0 & the.censor.codes > 0

if(printem) {
  
   out <- list()
   
   summary_title <- paste0("Summary of ", get.data.title(data.ld))
   row_names <- c(NULL)
   vals <- c(NULL)
  
   data.note <- get.data.note(data.ld)
   
   if(!is.null(data.note)) {
      
      the.characters <- string2char(data.note)
      print.note <- length(the.characters) > 1 && !all(the.characters == "")
      if(print.note) {
        
        row_names <- c(row_names, parse.note(data.note))
        vals <- c(vals,'')
        
      }
      
   }
   
   row_names = c(row_names,"Rows in data matrix: ") ; vals = c(vals,nrow(the.response))
   row_names = c(row_names,"Response units: "); vals = c(vals, get.time.units(data.ld))
   row_names = c(row_names,"Minimum response: "); vals = c(vals, format(min(the.response)))
   row_names = c(row_names,"Maximum response: "); vals = c(vals, format(max(the.response)))
   row_names = c(row_names,"Cases in data set: ") ; vals = c(vals, sum(the.case.weights[not.dummy]))
   
   number.exact.fail <- sum(the.case.weights[not.dummy & the.censor.codes == 1])
   
   if(number.exact.fail > 0) {
     
      row_names = c(row_names, "Exact observations: ")
      vals = c(vals, number.exact.fail)
     
   }
   
   number.right.censored <- sum(the.case.weights[not.dummy & the.censor.codes == 2])
   if(number.right.censored > 0) {
     
      row_names = c(row_names, "Right censored observations: ")
      vals = c(vals, number.right.censored)
     
   }
   
   number.left.censored <- sum(the.case.weights[not.dummy & the.censor.codes == 3])
   if(number.left.censored > 0) {
     
      row_names = c(row_names, "Left censored observations: ")
      vals = c(vals, number.left.censored)
     
   }
   
   number.interval.censored <- sum(the.case.weights[not.dummy & the.censor.codes == 4])
   if(number.interval.censored > 0) {
     
      row_names = c(row_names, "Interval censored observations: ")
      vals = c(vals, number.interval.censored)
     
   }
   
   number.sinterval.censored <- sum(the.case.weights[not.dummy & the.censor.codes == 5])
   if(number.sinterval.censored > 0) {
     
      row_names = c(row_names, "Small-interval observations: ") ; vals = c(vals, number.sinterval.censored)
     
   }
   
   if(no.censoring) {
     
      row_names = c(row_names, "Censoring information: ")
      vals = c(vals,'none')
     
   }
   
   the.failure.modes <- failure.modes(data.ld)
   if(!is.null(the.failure.modes)) {
     
      row_names = c(row_names, "Unique failure modes: ")
      vals = c(vals, paste(unique(as.character(the.failure.modes)),collapse = ", "))
     
   }
   
   truncation.codes <- truncation.codes(data.ld)
   ty <- truncation.response(data.ld)
   
   if(!is.null(truncation.codes) && !is.null(ty)) {
     
      if(is.null(truncation.codes) || is.null(ty)) {
        
         stop("If either truncation.codes or ty is specified, both must be specified")
        
      }
      if(length(truncation.codes) != number.cases) {
        
         stop(paste("Number of truncation codes ",length(truncation.codes), " is wrong"))
        
      }
      ty <- as.matrix(ty)
      if (nrow(ty) != number.cases) {
        
          stop(paste("Number of truncation times ",length(ty), " is wrong"))
        
      }
      nty <- ncol(ty)
      row_names = c(row_names, "Right truncated observations: ")
      vals = c(vals,  sum(the.case.weights[not.dummy & truncation.codes == 2]))
      row_names = c(row_names, "Left truncated observations: ") 
      vals = c(vals, sum(the.case.weights[not.dummy & truncation.codes == 3]))
      row_names = c(row_names, "Interval truncated observations: ")
      vals = c(vals, sum(the.case.weights[not.dummy & truncation.codes == 4]))
      
    } else {
      
      row_names = c(row_names, "Truncation information: ")
      vals = c(vals,'none')
      nty <- 0
      truncation.codes <- rep(1, length(the.censor.codes))
      ty <- rep(0, length(the.censor.codes))
      
    }
   
    the.xmat <- xmat(data.ld)
    if(is.null(the.xmat)) {
       
       row_names = c(row_names,"Explanatory variables: ")
       vals = c(vals,'none')
      
     } 
    
     out[[summary_title]] <- data.frame(row_names,
                                        vals, 
                                        row.names = NULL,
                                        stringsAsFactors = F) 
     
     colnames(out[[summary_title]]) <- NULL
    
    if(!is.null(the.xmat)) {
       
       if(nrow(the.xmat) < 200) {
         
          x.strings <- apply(the.xmat, 1, paste, collapse = " ")
          uniquex <- unique(x.strings)
          if (is.list(the.xmat)) {
            
              numeric.columns.list <- lapply(the.xmat, is.numeric)
              numeric.columns <- unlist(numeric.columns.list)
              
            } else {
              
              numeric.columns <- apply(the.xmat, 2, is.numeric)
              
            }
          
            if(any(numeric.columns)) {
              
               the.mean <- apply(the.xmat[, numeric.columns,drop = F], 2, mean)
               the.sd <- sqrt(apply(the.xmat[, numeric.columns, drop = F], 2, var))
               the.cv <- the.sd/the.mean
               predictors <- names(get.x.columns(data.ld)[numeric.columns])
               xsummary <- data.frame(predictors,
                                      apply(the.xmat[, numeric.columns,drop = F], 2, min), 
                                      apply(the.xmat[, numeric.columns,drop = F], 2, max), 
                                      the.mean, 
                                      the.sd, 
                                      the.cv,
                                      stringsAsFactors = F)

               colnames(xsummary) <- c("predictor","min", "max", "mean", "sd", "cv")
               out[["Summary of numeric columns in X matrix"]] <- xsummary
               
            }
          
            ncolx <- ncol(the.xmat)
            c1 <- rep(1, length(uniquex))
            the.table <- data.frame(the.xmat[1:length(uniquex),], c1, c1, c1, c1, c1, c1, c1, c1)
            
            for(i in 1:length(uniquex)) {
              
                the.stuff <- uniquex[i] == x.strings
                the.table[i, ncolx + 1] <- min(the.response[the.stuff])
                the.table[i, ncolx + 2] <- max(the.response[the.stuff])
                the.mean <- mean(the.response[the.stuff])
                if(min(the.response[the.stuff]) == max(the.response[the.stuff])) the.mean <- 0
                the.table[i, ncolx + 3] <- the.mean
                the.sd <- sqrt(var(the.response[the.stuff]))
                if (is.na(the.sd)) the.sd <- 0
                the.table[i, ncolx + 4] <- the.sd
                the.table[i, ncolx + 5] <- sum(the.case.weights[the.stuff & the.censor.codes == 1])
                the.table[i, ncolx + 6] <- sum(the.case.weights[the.stuff & the.censor.codes == 2])
                the.table[i, ncolx + 7] <- sum(the.case.weights[the.stuff & the.censor.codes == 3])
                the.table[i, ncolx + 8] <- sum(the.case.weights[the.stuff & the.censor.codes == 4])
                the.table[i, ncolx + 9] <- sum(the.case.weights[the.stuff])
                the.ones <- (1:nrow(the.response))[the.stuff]
                
                for(j in 1:ncolx) {
                  
                    if(is.factor(the.xmat[the.ones[1], j])) {
                      
                       the.table[i, j] <- as.character(the.xmat[the.ones[1],j])
                       
                     } else { 
                       
                       the.table[i, j] <- the.xmat[the.ones[1],j]  
                      
                     }
                  
                }
                    
           }
                unique_combinations <- 1:nrow(the.table)
                the.table <- cbind(unique_combinations, the.table)
                colnames(the.table) <- c("",
                                         names(get.x.columns(data.ld)),
                                         "Min response", 
                                         "Max response", 
                                         "Mean response",
                                         "SD response",
                                         "Exact", 
                                         "R-cen", 
                                         "L-cen", 
                                         "Int-cen",
                                         "Total")
                
                check.zero <- function(x) { any(x != 0) }
                some.censoring <- any(the.table[, ncolx + 1 + 9] != the.table[, ncolx + 1 + 5])
                if(!some.censoring) the.table <- the.table[, -(ncolx + 1 + c(6, 7, 8))]
                any.non.zero <- apply(the.table, 2, check.zero)
                
                if(nrow(the.table) < print.limit) { 
                  
                   out[["Unique X conditions"]] <- the.table[, any.non.zero]
                   
                 } else {
                   
                   out[["Unique X conditions"]] <- paste0("Unique X conditions: ", nrow(the.table))
                   
                 }
            }
      }
}

    print(out, row.names = F)

    results <- list(number.cases = sum(the.case.weights[not.dummy]))
    invisible(results)
            
}