#' Title
#'
#' @param data.ld 
#' @param distribution 
#' @param relationship.parameters 
#' @param explan.vars 
#' @param gamthr 
#' @param theta.start 
#' @param parameter.fixed 
#' @param intercept 
#' @param kprint 
#' @param maxit 
#' @param debug1 
#' @param genforce 
#' @param ... 
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' berkson200.ld <- frame.to.ld(berkson200,
#'                              response.column = c(1,2),
#'                              censor.column = 3,
#'                              case.weight.column = 4,
#'                              time.units = "1/5000 Seconds")
#' 
#' summary(berkson200.ld)
#' 
#' plot(berkson200.ld)
#' plot(berkson200.ld, dist = "Exponential")
#' 
#' cdfest(berkson200.ld)
#' 
#' berkson200.mle.exp <- mlest(berkson200.ld, 
#'                             distribution = "Exponential")
#' 
#' print(berkson200.mle.exp)
#' 
#' doatrun.ld <- frame.to.ld(doatrun,
#'                           response.column = c(1,2),
#'                           censor.column = 3, 
#'                           case.weight.column = 4,
#'                           truncation.response.column = 5, 
#'                           truncation.type.column = 6, 
#'                           data.title = "DOA Truncated Data", 
#'                           time.units = "Hours")
#' 
#' summary(doatrun.ld)
#' 
#' cdfest(doatrun.ld)
#' 
#' doatrun.mle.weib <- mlest(doatrun.ld,"Weibull")
#' 
#' print(doatrun.mle.weib)
#' }
mlest <-
function (data.ld, 
          distribution, 
          relationship.parameters = 1,
          explan.vars = NULL, 
          gamthr = 0, 
          theta.start = NULL, 
          parameter.fixed = NULL,
          intercept = T, 
          kprint = 0, 
          maxit = 500,
          debug1 = F, 
          genforce = F,
          conf.level = GetSMRDDefault("SMRD.ConfLevel")/100,
          digits = GetSMRDDefault("SMRD.DigitsPrinted"), 
          print.vcv = GetSMRDDefault("SMRD.LongPrint"),
          add.title = NULL, 
          quote = T, 
          prefix = "",...)
{
    simple.distribution <- (numdist(distribution) <= 10 && numdist(distribution) >=
        0) || numdist(distribution) == 14
    
    simple.regr.model <- (is.atomic(explan.vars) || is.null(explan.vars)) &&
        relationship.parameters == 1
    
    simple.model <- simple.distribution && simple.regr.model
    
    if (simple.model && !genforce) {
      
        mlest.out = star2.mlest(data.ld = data.ld, 
                                distribution = distribution,
                                explan.vars = explan.vars, 
                                gamthr = gamthr, 
                                theta.start = theta.start,
                                parameter.fixed = parameter.fixed, 
                                intercept = intercept,
                                kprint = kprint,
                                debug1 = debug1,...)
    } else {
  
        mlest.out = gmlest(data.ld, 
                           distribution, 
                           theta.start = theta.start,
                           parameter.fixed = parameter.fixed, 
                           kprint = kprint,
                           explan.vars = explan.vars,...)
        
    }
    
        variable.names <- period.to.paren(dimnames(mlest.out$vcv.matrix)[[1]])
    dimnames(mlest.out$vcv.matrix) <- list(variable.names, variable.names)
    names(mlest.out$theta.hat) <- variable.names
    if (names(mlest.out$theta.hat)[1] == "b0")
        names(mlest.out$theta.hat)[1] <- "Intercept"
    old.options <- options(digits = digits)
    on.exit(options(old.options))
    line1<-paste(get.data.title(mlest.out$data), add.title, sep = " ")
    line2<-"Maximum likelihood estimation results:"
    line3<-paste("Response units:", get.time.units(mlest.out$data.ld), sep = "")
    line4<-paste(distribution.name(mlest.out$distribution),
                 " Distribution", sep = "")
    
    if (!is.null(mlest.out$relationship)) {
      
        group.var <- mlest.out$class.group
        if (is.null(group.var)) group.var <- mlest.out$group.var
        the.names <- dimnames(xmat(mlest.out$data.ld)[, group.var,  drop = F])[[2]]
        relat.names <- name.relationship(mlest.out$relationship)
        arrhenius.names <- ifelse(relat.names == "Arrhenius",
                                  SMRDOptions("SMRD.Boltzmann"), "")
        the.table <- matrix(paste(the.names, paste(relat.names, arrhenius.names), sep = ": "), ncol = 1)
        dimnames(the.table) <- list(1:nrow(the.table), "")
        cat("Variable: Relationship (g)")
        print(the.table, quote = F)
        contrast.method <- attr(mlest.out$the.model.matrix, "contrast.method")
        if (any(multiple.generic.relationship.name(mlest.out$relationship) ==
            "class") && !is.null(contrast.method))
            cat("\nS-PLUS dummy variable coding method used for Class variable(s):\n",
                paste(contrast.method, collapse = ","), "\n")
        DecodeDummyVariables(mlest.out)
        cat("\nModel formula:\n")
        
        print(deparse(mlest.out[[1]]$terms))
        cat("\n")
    }
    LL.result<-paste("The maximum value of the log-likelihood function for the",
              get.data.title(mlest.out$data), add.title, "(assuming the", mlest.out$distribution,
"distribution is correct) was found to be", format(mlest.out$log.likelihood), sep = " ")
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
            lower[i] <- param[i]/exp((zquant * se[i])/param[i])
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

    mttf.text  <- mttf(mlest.out)
    mttf.value <- mttf(mlest.out)$fun.hat
    fail.prob  <- signif(failure.probabilities(mlest.out), digits = digits)
    quantiles  <- signif(quantiles(mlest.out), digits = digits)
    pcm        <- ccor(mlest.out$vcv.matrix)
    
    xlim <- range(Response(mlest.out$data.ld))
    
    `if`(is.logdist(mlest.out$distribution),
         time.vec <- as.numeric(logax(xlim)$ticlab),
         time.vec <- as.numeric(linax(xlim)$ticlab))
  

    hazard <- plot.hazard(mlest.out, plotem = F, time.vec = time.vec)
    
    if (is.logdist(mlest.out$distribution))  hazard[,1] <- exp(hazard[,1])

    mlest.out$ll.text <- LL.result
    mlest.out$ll.value <- mlest.out$log.likelihood
    mlest.out$mttf.text <- capture.output(cat(print(mttf.text)))[[2]]
    mlest.out$mttf.value <- mttf.value
    mlest.out$mle.table <- esti.matrix
    mlest.out$vcv.matrix <- mlest.out$vcv.matrix
    mlest.out$param.corr.matrix <- pcm
    mlest.out$failure.probabilities <- fail.prob
    mlest.out$quantiles <- quantiles
    mlest.out$hazard.table <- hazard
    
    return(mlest.out)

}
