predict.order.posterior <-
function (object, 
          nsamsize, 
          prior.and.posterior, 
          time.range = NULL,
          x.of.interest = NULL, 
          extra.explan.vars = NULL, 
          use.rows = 1:length(prior.and.posterior$post[, 1]), 
          pred.tail = 0.001,
          number.time.points = 100,...)
{
    use.rows.prior <- use.rows
    
    if (is.null(nrow(prior.and.posterior$prior)))
        prior.and.posterior$prior <- prior.and.posterior$prior$prior
    
    if (length(use.rows.prior) > nrow(prior.and.posterior$prior))
        use.rows.prior <- 1:length(prior.and.posterior$prior[, 1])
    
    prior.and.posterior$prior <- 
      as.matrix(prior.and.posterior$prior[use.rows.prior,])
    
    prior.and.posterior$post <- 
      as.matrix(prior.and.posterior$post[use.rows,])
    
    prior.and.posterior$prior[, 1] <- 
      logb(prior.and.posterior$prior[,1])
    
    prior.and.posterior$post[, 1] <- 
      logb(prior.and.posterior$post[,1])
    
    distribution <- prior.and.posterior$distribution
    explan.vars <- c(prior.and.posterior$explan.vars, 
                     extra.explan.vars)
    
    if (!is.null(explan.vars)) {
      
        for (i in explan.vars) {
          
            prior.and.posterior$xs[i] <- 
              f.relationship(prior.and.posterior$xs[i],
                             prior.and.posterior$relationships[i])
            
            x.of.interest[i] <- 
              f.relationship(x.of.interest[i],
                             prior.and.posterior$relationships[i])
        }
      
        xs.plus <- c(prior.and.posterior$xs[explan.vars], 
                     quant(prior.and.posterior$p, distribution))
        
        col.use <- c((explan.vars + 1), 
                     ncol(prior.and.posterior$prior))
    } else {
      
        col.use <- c(ncol(prior.and.posterior$prior))
        xs.plus <- c(quant(prior.and.posterior$p, distribution))
    }
    
    prior.and.posterior$prior[, 1] <- 
      prior.and.posterior$prior[,1] - 
      as.matrix(prior.and.posterior$prior[, col.use]) %*% xs.plus
    
    prior.and.posterior$post[, 1] <- 
      prior.and.posterior$post[,1] - 
      as.matrix(prior.and.posterior$post[, col.use]) %*% xs.plus
    
    col.use <- col.use[1:(length(col.use) - 1)]
    
    if (!is.null(explan.vars)) {
      
        mu.at.x.of.interest.prior <- 
          prior.and.posterior$prior[,1] + 
          as.matrix(prior.and.posterior$prior[, col.use]) %*%
          x.of.interest
        
        mu.at.x.of.interest.post <- 
          prior.and.posterior$post[,1] + 
          as.matrix(prior.and.posterior$post[, col.use]) %*%
          x.of.interest
        
    } else {
      
        mu.at.x.of.interest.prior <- 
          prior.and.posterior$prior[, 1]
        
        mu.at.x.of.interest.post <- 
          prior.and.posterior$post[, 1]
    }
    
    if (is.null(time.range)) {
      
        muaverage <- mean(mu.at.x.of.interest.post)
        sigmaaverage <- mean(prior.and.posterior$post[, ncol(prior.and.posterior$post)])
        time.range <- c(muaverage + quant(pred.tail, distribution) *
            sigmaaverage, muaverage + quant(1 - pred.tail, distribution) *
            sigmaaverage)
        
    }
    
    timevec <- seq(time.range[1], 
                   time.range[2], 
                   length = number.time.points)
    
    prediction.order.post <- 
      prediction.order.average(object,
                               nsamsize, 
                               timevec, 
                               mu.at.x.of.interest.post, 
                               prior.and.posterior$post[,
            ncol(prior.and.posterior$post)], 
                               distribution)
    return(prediction.order.post)
}
