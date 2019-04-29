#' Bootstrap based prediction intervals 
#'
#' @param B The number of bootstrap cycles
#' @param N The size of the original data sample
#' @param mu ML estimate for the mean of the original data sample
#' @param sigma ML estimate for the standard deviation of the original data sample
#' @param t_c Censoring time for Type-I censored tests
#' @param fails Number of failures for Type-II censored tests
#' @param alpha Desired significance level
#' 
#' @export
boot.pivot <- 
  function(B = 100, N = 20, 
           mu = 5.13, sigma = 0.161, 
           t_c = NULL, fails = NULL, alpha = 0.05) 
{

if(!is.null(t_c) && !is.null(fails)) { 
  
  stop('For censored tests, specify either the\n
       time "t_c" or the number of failures "fails"\n
       when testing ends -- not both')
  
}
    
samp <- replicate(B, rlnorm(N, mu, sigma))

samp <- apply(samp, MARGIN = 2, sort)

if(!is.null(t_c))   samp[which(samp > t_c)] <- t_c
if(!is.null(fails)) { 

samp[fails:N,] <- matrix(rep(samp[fails,],each = N-fails+1), 
                         ncol = B, 
                         byrow = F) 
}

params <- 
  sapply(X = 1:B, 
         FUN = function(x) {
           
      right <- N - fails
      samp.df <- 
        data.frame(samp[,x],
                   rep(c('f','r'),c(fails,right)))
      samp.ld <- 
        frame.to.ld(samp.df,
                    response.column = 1,
                    censor.column = 2)
      
      print(mlest(samp.ld, distribution = 'lognormal'))$mle[,1]
})

zlog_t <- 
  sapply(X = 1:B,
        FUN = function(x) { 
                        
    numer <- log(rlnorm(1, mu, sigma)) - params[[1,x]]
    denom <- params[[2,x]]
    numer / denom
})

zlog_t <- sort(zlog_t)

limits <- zlog_t[c(B * alpha / 2, B * (1 - alpha / 2))]

zout         <- list()
zout$zlog_t  <- zlog_t
zout$limits  <- limits 
zout$predict <- exp(mu + limits * sigma) 

return(zout)
  }
