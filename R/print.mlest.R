#' @export
print.mlest <-
function (x,
          conf.level = GetSMRDDefault("SMRD.ConfLevel")/100,
          digits = GetSMRDDefault("SMRD.DigitsPrinted"),
          print.vcv = GetSMRDDefault("SMRD.LongPrint"),
          add.title = NULL,
          quote = T,
          prefix = "",
          printem = T,...)
{
    mlest.out <- x
    variable.names <- period.to.paren(dimnames(mlest.out$vcv.matrix)[[1]])
    dimnames(mlest.out$vcv.matrix) <- list(variable.names, variable.names)
    names(mlest.out$theta.hat) <- variable.names
    if (names(mlest.out$theta.hat)[1] == "b0")
        names(mlest.out$theta.hat)[1] <- "Intercept"
    old.options <- options(digits = digits)
    on.exit(options(old.options))
# cat("\n\n")
# cat(      "Data:           ", paste(get.data.title(mlest.out$data), add.title, sep = ""))
# cat("\n")
# cat(paste("Response units: ", get.time.units(mlest.out$data.ld), sep = ""))
# cat("\n")
# cat(paste("Distribution:   ", distribution.name(mlest.out$distribution), sep = ""))
# cat("\n\n")
#     
    if (!is.null(mlest.out$relationship)) {

        group.var <- mlest.out$class.group
        if (is.null(group.var)) group.var <- mlest.out$group.var
        the.names <- dimnames(xmat(mlest.out$data.ld)[, group.var,  drop = F])[[2]]
        relat.names <- name.relationship(mlest.out$relationship)
        arrhenius.names <- `if`(relat.names == "Arrhenius",
                                SMRDOptions("SMRD.Boltzmann"), 
                                "")
        
        the.table <- matrix(paste(the.names, 
                                  paste(relat.names, arrhenius.names), 
                                  sep = ": "), 
                            ncol = 1)
        dimnames(the.table) <- list(1:nrow(the.table), "")
        cat("Variable: Relationship (g)")
        print(the.table, quote = F)
        contrast.method <- attr(mlest.out$the.model.matrix, "contrast.method")
        
        if (any(multiple.generic.relationship.name(mlest.out$relationship) ==
            "class") && !is.null(contrast.method)) {
          
            cat("\nS-PLUS dummy variable coding method used for Class variable(s):\n",
                paste(contrast.method, collapse = ","), "\n")
          
        }
        DecodeDummyVariables(mlest.out)
        
        cat("\nModel formula:\n") ; print(deparse(mlest.out$terms)) ; cat("\n")
    }
    
    LL.result<-paste("The maximum value of the log-likelihood function for the",
                     get.data.title(mlest.out$data), 
                     add.title, 
                     "(assuming the", 
                     mlest.out$distribution,
                     "distribution is correct) was found to be", 
                     format(mlest.out$log.likelihood), sep = " ")
    param <- mlest.out$theta.hat
    se <- sqrt(diag(mlest.out$vcv.matrix))
    kodet <- mlest.out$kodet
    hold.names <- names(param)
    
    if (generic.distribution(mlest.out$distribution) == "weibull") {
      
        if (length(param) == 2 && hold.names[1] == "mu") {
            param <- c(param, exp(param[1]), 1/param[2])
            se <- c(se, se[1] * param[3], se[2]/param[2]^2)
            kodet <- c(kodet, 2, 2)
            names(param) <- c(hold.names, "Weibull (eta)", "Weibull (beta)")
            
      } else {
        
            if (kodet[length(param)] == 2) {
              
                se <- c(se, se[length(param)]/param[length(param)]^2)
                param <- c(param, beta = 1/param[length(param)])
                kodet <- c(kodet, 2)
                names(param) <- c(hold.names, "Weibull (beta)")
                
            }
        
      }
      
    }
    
    if (generic.distribution(mlest.out$distribution) == "exponential") {
      
        if (length(param) == 2 && hold.names[1] == "mu") {
          
            param <- c(param, exp(param[1]))
            se <- c(se, se[1] * param[3])
            kodet <- c(kodet, 2, 2)
            names(param) <- c(hold.names, "Exponential (mean)")
            
        }
      
    }
    
    zquant <- qnorm(1 - (1 - conf.level)/2)
    lower <- param
    upper <- param
    
    for (i in 1:length(param)) {
      
        switch(kodet[i], {
            lower[i] <- param[i] - zquant * se[i]
            upper[i] <- param[i] + zquant * se[i]
        }, {
            lower[i] <- param[i] / exp((zquant * se[i])/param[i])
            upper[i] <- param[i] * exp((zquant * se[i])/param[i])
        }, )
      
    }
    
    esti.matrix <- cbind(param, se, lower, upper)
    conf.char <- percent.conf.level(conf.level)
    indent <- max(nchar(dimnames(esti.matrix)[[1]]))
    dimnames(esti.matrix)[2] <- list(c("MLE", 
                                       "Std.Err.", 
                                       paste(conf.char,"Lower"), 
                                       paste(conf.char, "Upper")))
    
    skip.space <- paste(rep(" ", indent + 7), collapse = "")

    mttf.text <- mttf(mlest.out)
    mttf.value <- mttf(mlest.out)$fun.hat
    fail.prob <- signif(failure.probabilities(mlest.out), digits = digits)
    quantiles <- signif(quantiles(mlest.out), digits = digits)
    pcm       <- ccor(mlest.out$vcv.matrix)

    xlim <- range(Response(mlest.out$data.ld))

    `if`(is.logdist(mlest.out$distribution),
         time.vec <- as.numeric(logax(xlim)$ticlab),
         time.vec <- as.numeric(linax(xlim)$ticlab))


    hazard     <- plot.hazard(mlest.out, plotem = F, time.vec = time.vec)

    if (is.logdist(mlest.out$distribution))  hazard[,1] <- exp(hazard[,1])

    x$ll.text <- LL.result
    x$ll.value <- mlest.out$log.likelihood
    x$mttf.text <- capture.output(cat(print(mttf.text)))[[2]]
    x$mttf.value <- mttf.value
    x$mle.table <- esti.matrix
    x$vcv.matrix <- mlest.out$vcv.matrix
    x$param.corr.matrix <- pcm
    x$failure.probabilities <- fail.prob
    x$quantiles <- quantiles
    x$hazard.table <- hazard
    
    if(printem) {
    
       cat("\nMaximum likelihood estimation results")
       cat("\n")
       cat("---------------------------------------\n\n")
       cat(x$ll.text)
       cat("\n\n\n")
       cat(x$mttf.text)
       cat("\n\n\n")      
       cat("ML estimate table:\n")
       cat("-------------------------------------\n\n")
       print(x$mle.table)
       cat("\n\n")      
       cat("Variance covariance matrix:\n")
       cat("-------------------------------------\n\n")      
       print(x$vcv.matrix)
       cat("\n\n")
       cat("Parameter correlation matrix:\n")
       cat("-------------------------------------\n\n")      
       print(x$param.corr.matrix)
       cat("\n\n")      
       cat("CDF table:\n")
       cat("-------------------------------------\n\n")      
       print(x$failure.probabilities)
       cat("\n\n")      
       cat("Quantile function table:\n")
       cat("-------------------------------------\n\n")      
       print(x$quantiles)
       cat("\n\n")      
       cat("Hazard function table:\n")
       cat("-------------------------------------\n\n")      
       print(x$hazard.table)
       cat("\n")
      
    }
    
    invisible(x)
      
}
