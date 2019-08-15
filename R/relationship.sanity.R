relationship.sanity <-
function (the.xmat, 
          relationship, 
          relat.string = "Relationship")
{
    problem <- F
    xnames <- dimnames(the.xmat)[[2]]
    
    if (length(relationship) != length(xnames)) {
        
        stop(paste("\n***The number of relationships", 
                   length(relationship),
                   " is not equal to", length(xnames), 
                   "(the number of columns in the X matrix)\n",
                   paste(xnames, collapse = ", "), 
                   "\n", 
                   paste(relationship, collapse = ", ")))
        
    }
    
    for (i in 1:length(relationship)) {
        
        the.x <- the.xmat[, i]
        switch(generic.relationship.name(relationship[i]), invtemp = ,
            invtemp3 = , Arrhenius = , Eyring = , Arrhenius3 = {
                badx <- the.x < -273.16
                appropriate.values <- "> -273.16"
            }, `Box-Cox` = , reciprocal = , log = , log10 = {
                badx <- the.x <= 0
                appropriate.values <- "> 0."
            }, squareroot = {
                badx <- the.x < 0
                appropriate.values <- ">=0."
            }, humidity = {
                badx <- the.x <= 0 | the.x >= 100
                appropriate.values <- "between 0 and 100."
            }, logit2 = , logit = {
                badx <- the.x <= 0 | the.x >= 1
                appropriate.values <- "between 0 and 1."
            }, {
                badx <- F
            })
        
        if (any(badx)) {
            
            warning(paste("X out of range problem with",
                          relat.string, 
                          relationship[i], 
                          "\n    in column/position",
                          i, 
                          "with variable name", 
                          xnames[i], 
                          "\n Appropriate values for x variable are",
                          appropriate.values, "\n"))
            
            problem <- T
            positions <- (1:length(the.x))[badx]
            the.bad.x <- the.x[badx]
            names(the.bad.x) <- as.character(positions)
            
            if (length(the.bad.x) <= 100) print(the.bad.x)
            
        }
        
    }
    
    if (problem) stop("\nCheck the relationships for the offending variable(s) and rerun.")
    
    invisible(NULL)
}
