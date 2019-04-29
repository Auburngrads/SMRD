get.big.posterior <-
function (specifications.for.prior, 
          data.ld = NULL, 
          explan.vars = NULL,
          number.in.init.prior = number.needed, 
          number.needed = 4000)
{
    if (is.null(specifications.for.prior))
        stop("prior spec must be given to make a posterior")
  
    number.in.old.post <- 0
    distribution <- attr(specifications.for.prior, "distribution")
    initial.prior <- make.prior(specifications.for.prior, 
                                number.in.init.prior)
    
    new.prior.and.post <- compute.posterior(initial.prior, 
                                            data.ld,
                                            distribution, 
                                            explan.vars)
    
    if (length(new.prior.and.post$post) == 0)
        stop("Number in initial posterior is 0; check that there is some prior/likelihood overlap")
    
    number.obtained.in.post <- nrow(new.prior.and.post$post)
    fraction.obtained.in.post <- number.obtained.in.post/number.in.init.prior
    
    if (fraction.obtained.in.post < 0.005)
        warning(paste("Due to difuseness in prior, fraction of prior obtained in the posterior is only",
            fraction.obtained.in.post))
    old.prior.and.post <- new.prior.and.post
    
    number.iter <- 10
    number.in.prior <- ceiling((1.1 * (number.needed - number.obtained.in.post))/(number.iter *
        fraction.obtained.in.post))

add.to.posterior <- function (old.prior.and.post = NULL, new.prior.and.post)
     {
        if (!is.null(old.prior.and.post)) {
          xnew.prior.and.post <- new.prior.and.post
          xnew.prior.and.post$post <- rbind(old.prior.and.post$post,
                                            new.prior.and.post$post)
          oldClass(xnew.prior.and.post) <- "prior.and.post"
          return(xnew.prior.and.post)
          
        } else {
          
          oldClass(new.prior.and.post) <- "prior.and.post"
          return(new.prior.and.post)
          
        }
      }

    for (i in 1:number.iter) {
        if (nrow(old.prior.and.post$post) > number.needed) break
      
        prior <- make.prior(specifications.for.prior, number.in.prior)
        
        new.prior.and.post <- compute.posterior(prior, 
                                                data.ld,
                                                distribution, 
                                                explan.vars)
        
        old.prior.and.post <- add.to.posterior(old.prior.and.post,
                                               new.prior.and.post)
        
        cat("\nIter", i, " of ", number.iter, "Number in post now=",
            nrow(old.prior.and.post$post), "of ", number.needed,
            "\n")
    }
    old.prior.and.post$prior <- initial.prior
    the.prior.and.post <- list(post = old.prior.and.post$post,
        specifications.for.prior = old.prior.and.post$prior$specifications.for.prior,
        prior = old.prior.and.post$prior$prior, xs = old.prior.and.post$xs,
        p = old.prior.and.post$p, relationships = old.prior.and.post$relationships,
        explan.vars = old.prior.and.post$explan.vars, distribution = old.prior.and.post$distribution,
        data.ld = old.prior.and.post$data.ld)
    oldClass(the.prior.and.post) <- "posterior"
    return(the.prior.and.post)
}
