compute.posterior <-
function (prior, 
          data.ld, 
          distribution, 
          explan.vars = NULL) 
{
    prior.temp <- prior
    orig.data.ld <- data.ld
    prior.temp$prior[, 1] <- logb(prior.temp$prior[, 1])
    
    if (!is.null(explan.vars)) {
      
        relationships <- 
          set.relationship.power(prior.temp$relationships, 
                                 power)
        x.columns <- attr(data.ld, "x.columns")
        
        for (i in explan.vars) {
          
            the.relationship <- subscript.relationship(relationships, i)
            
            data.ld[, x.columns[i]] <- 
              f.relationship(data.ld[, x.columns[i]], the.relationship)
            
            prior.temp$xs[i] <- 
              f.relationship(prior.temp$xs[i], the.relationship)
            
        }
        
    } else { relationships <- NULL }
    
    mlest.out <- mlest(data.ld, distribution, explan.vars = explan.vars)
    xs.plus <- c(prior.temp$xs[explan.vars], 
                 quant(prior.temp$p, distribution))
    
    `if`(!is.null(explan.vars),
         col.use <- c((explan.vars + 1), ncol(prior.temp$prior)),
         col.use <- c(ncol(prior.temp$prior)))
    
    prior.temp$prior[, 1] <- 
      prior.temp$prior[, 1] - as.matrix(prior.temp$prior[, col.use]) %*% xs.plus
    
    mode(prior.temp$prior) <- "single"
    accept.probs <- 
      exp(like.eval(data.ld, 
                    distribution, 
                    prior.temp$prior[, c(1, col.use)], 
                    explan.vars = explan.vars) - mlest.out$log.likelihood)
    
    post <- prior.temp$prior[runif(length(accept.probs)) < accept.probs, 
        c(1, col.use)]
    
    if (is.null(nrow(post))) {
        warning("nothing left in the posterior---check your prior")
        post <- NULL
        
        } else {
          
        post[, 1] <- post[, 1] + as.matrix(post[, col.use]) %*% xs.plus
        post[, 1] <- exp(post[, 1])
        
        if (F) { 
          cat(paste("\nThe posterior sample has", nrow(post), "points.",
                    "To increase the size of the posterior sample",
                    "by a factor of x, multiply the prior sample size",
                    "by a factor of x or use get.big.posterior().\n",
                    sep = '\n'))
          }
        }
    
    mode(post) <- "single"
    bayes.out <- list(prior = prior, 
                      post = post, 
                      xs = prior$xs, 
                      p = prior$p, 
                      relationships = relationships, 
                      explan.vars = explan.vars, 
                      distribution = distribution, 
                      data.ld = orig.data.ld)
    
    oldClass(bayes.out) <- "prior.and.post"
    invisible(bayes.out)
}
