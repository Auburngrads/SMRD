mlestlev <-
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
          genforce = F,...)
{
    orig.distribution <- distribution
    orig.data.ld <- data.ld
    
    if (is.element(generic.distribution(distribution), c("lev", "frechet"))) {
      
        truncation.column <- attr(data.ld, "truncation.type.column")
        
        if (!is.null(truncation.column)) {
          
            the.truncation.status <- truncation.status(data.ld)
            new.trun.status <- as.character(data.ld[[truncation.column]])
            new.trun.status[is.element(the.truncation.status,
                c("right", "Right"))] <- "Left"
            new.trun.status[is.element(the.truncation.status,
                c("left", "Left"))] <- "Right"
            
            data.ld[truncation.column] <- factor(new.trun.status)
            truncation.response.column <- attr(data.ld, "truncation.response.column")
            the.truncation.response <- truncation.response(data.ld)
            ny <- ncol(the.truncation.response)
            
            if (is.logdist(distribution)) {
              
                the.truncation.response <- 1/the.truncation.response
                the.truncation.response[the.truncation.response == Inf] <- 0
                TruncationResponse(data.ld) <- the.truncation.response[, c(ny, 1)]
                
          } else {
            
                TruncationResponse(data.ld) <- -the.truncation.response[, c(ny, 1)]
            
                }
        }
        
        censor.column <- attr(data.ld, "censor.column")
        the.reverse.codes <- Reverse.Codes(data.ld)
        
        if (!is.null(the.reverse.codes))
            data.ld[censor.column] <- Reverse.Codes(data.ld)
        
        the.response <- Response(data.ld)
        ny <- ncol(the.response)
        
        if (is.logdist(distribution)) {
          
            distribution <- "Weibull"
            Response(data.ld) <- 1/the.response[, c(ny, 1)]
            
      } else {
        
            Response(data.ld) <- -the.response[, c(ny, 1)]
            distribution <- "sev"
        
            }
    }
    
    assign(envir = .frame0, inherits = !TRUE,"tmpdoadummy.ld", data.ld)
    
    if (!is.null(theta.start))
        theta.start[-length(theta.start)] <- -theta.start[-length(theta.start)]
    
    result <- mlest(data.ld = data.ld, 
                    distribution = distribution,
                    relationship.parameters = relationship.parameters, 
                    explan.vars = explan.vars,
                    gamthr = gamthr, 
                    theta.start = theta.start, 
                    parameter.fixed = parameter.fixed,
                    intercept = intercept, 
                    kprint = kprint, 
                    maxit = maxit,
                    debug1 = debug1, 
                    genforce = genforce, ...)
    
    if (is.element(generic.distribution(orig.distribution), c("lev", "frechet"))) {
      
        result$data.ld <- orig.data.ld
        result$theta.hat[-length(result$theta.hat)] <- -result$theta.hat[-length(result$theta.hat)]
        result$distribution <- orig.distribution
        
    }
    
    result
}
