#' Create a four-panel distribution plot
#'
#' @param distribution Generates an array of ggplot2 grobs showing
#'                     the cdf, pdf, survival function and hazard 
#'                     function for a distribution
#' @param shape A vector of shape parameters
#' @param prob.range A vector (length 2) of values between 0 and 1 
#'                   providing the lower and upper limits of the 
#'                   probability range
#' @param number.points The number points to be used in the plot
#' @param scale A numeric value of the scale parameter (see Details)
#' @param location A numeric value of the location parameter (see Details)
#' @param shape2 A numeric value of the second shape parameter (see Details)
#' @param exponential2
#' @param cex A positive numeric value giving the amount by which the
#'            plot text should be magnified relative to the default.
#' @param lwd A positive numeric value giving the amount by which the
#'            line widths should be magnified relative to the default. 
#' @param ... Currently not used
#'
#' @importFrom ggplot2 ggplot geom_line aes theme_bw facet_wrap
#' @importFrom stats qlogis dlogis plogis
#' @importFrom stats qnorm dnorm pnorm
#' @importFrom stats qlnorm dlnorm plnorm
#' @importFrom stats qweibull dweibull pweibull
#' @importFrom stats qexp dexp pexp
#' @importFrom stats qgamma dgamma pgamma
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' distribution.plot2("Exponential",
#'                   shape = c(.5,1))
#' 
#' distribution.plot2("Lognormal",
#'                   shape = c(.3, .8))
#' 
#' distribution.plot2("Normal",
#'                   shape = c( .30, .5,.8),
#'                   location = 5)
#' 
#' distribution.plot2("Weibull",
#'                   shape = c(.8,1,1.5))
#' 
#' distribution.plot2("Smallest Extreme Value",
#'                   shape = c(5,6,7),
#'                   location = 50)
#' 
#' distribution.plot2("Largest Extreme Value",
#'                   shape = c(5, 6, 7),
#'                   location = 10)
#' 
#' distribution.plot2("Logistic",
#'                   shape = c(1, 2, 3),
#'                   location = 15)
#' 
#' distribution.plot2("Loglogistic",
#'                   shape = c(.2,.4,.6), 
#'                   prob.range = c(0.001, 0.95))
#' }
distribution.plot2 <-
  function (distribution, 
            shape, 
            prob.range = c(0.01, 0.99), 
            number.points = 1000,
            scale = rep(1, length(shape)), 
            location = rep(0,length(shape)), 
            shape2 = 1, 
            exponential2 = T,
            cex = 16,
            lwd = 1.5,...)
  {
    invisible()
    
    if (is.null(scale)) scale <- rep(1, max(1, length(shape)))
    
        char.eta <-   "\u03B7"
        char.mu <-    "\u03BC"
        char.nu <-    "\u03BD"
        char.sigma <- "\u03C3"
        char.kappa <- "\u03BA"
        char.theta <- "\u03B8"
        char.zeta <-  "\u03B6"
        char.gamma <- "\u03B3"
        char.beta <-  "\u03B2"
        char.xi <-    "\u03BE"
        parameter2.name <- NULL
        parameter1 <- shape
        weibhaz <- function(x, shape){
          
          return((shape / 1) * (x / 1)**(shape - 1))
          
        }
    
switch(SMRD:::generic.distribution(distribution), 
             
             exponential = {
               
             range <- c(min(0,qexp(prob.range[1],     shape) + location), 
                        max(qexp(prob.range[2], 1 / shape)) + location)
             x <- seq(range[1], range[2], length = number.points)
             
             pdf.x = x
             pdf.y = outer(x - location, 1/shape, dexp)
             cdf.x = x
             cdf.y = outer(x - location, 1/shape, pexp)
             haz.x = x
             haz.y = outer(x - location, 1/shape, dexp)/(1-outer(x - location,1/shape, pexp))
             rel.x = x
             rel.y = 1-outer(x - location, 1/shape, pexp)
             
             parameter2 <- location
             parameter1.name <- char.theta
             parameter2.name <- char.gamma
        
             p.string <- paste(parameter1.name,',',
                               parameter2.name,' = ',
                               format(parameter1, nsmall = 2),',',
                               format(parameter2, nsmall = 2), sep = '')
        
          }, weibull = {
        
             range <- c(min(qweibull(prob.range[1], shape, scale = scale)), 
                        max(qweibull(prob.range[2], shape, scale = scale)))
             x <- seq(range[1], range[2], length = number.points)
             pdf.x = x
             pdf.y = outer(x, shape, dweibull)
             cdf.x = x 
             cdf.y = outer(x, shape, pweibull)
             haz.x = x
             haz.y = outer(x, shape, weibhaz)#/(SMRD:::dist.outer((1/x), shape, dummy = scale, pweibull))
             rel.x = x
             rel.y = 1-outer(x, shape, pweibull)
             
             parameter2 <- scale
             parameter1.name <- char.beta
             parameter2.name <- char.eta
             
             p.string <- paste(parameter1.name,',',
                               parameter2.name,' = ',
                               format(parameter1, nsmall = 2),',',
                               format(parameter2, nsmall = 2), sep = '')
             
          }, sev = {
        
             range <- c(min(qsev(prob.range[1], location = location, shape)), 
                        max(qsev(prob.range[2], location = location, shape)))
             x <- seq(range[1], range[2], length = number.points)
             
             pdf.x = x
             pdf.y = SMRD:::dist.outer(x, shape, dummy = location,dsev)
             cdf.x = x 
             cdf.y = SMRD:::dist.outer(x, shape, dummy = location, psev)
             haz.x = x 
             haz.y = SMRD:::dist.outer(x, shape, dummy = location,dsev)/(SMRD:::dist.outer(x, shape, dummy = location,ssev)) 
             rel.x = x
             rel.y = SMRD:::dist.outer(x, shape, dummy = location, ssev)
             
             parameter2 <- location
             parameter2.name <- char.mu
             parameter1.name <- char.sigma
             
             p.string <- paste(parameter1.name,',',
                               parameter2.name,' = ',
                               format(parameter1, nsmall = 2),',',
                               format(parameter2, nsmall = 2), sep = '')
             
        
          }, lev = {
            
             range <- c(min(qlev(prob.range[1], location = location, shape)), 
                        max(qlev(prob.range[2], location = location, shape)))
             x <- seq(range[1], range[2], length = number.points)
             
             pdf.x = x 
             pdf.y = SMRD:::dist.outer(x, shape, dummy = location, dlev)
             cdf.x = x 
             cdf.y = SMRD:::dist.outer(x, shape, dummy = location, plev)
             haz.x = x 
             haz.y = SMRD:::dist.outer(x, shape, dummy = location,dlev)/(SMRD:::dist.outer(x, shape, dummy = location,slev))
             rel.x = x
             rel.y = SMRD:::dist.outer(x, shape, dummy = location, slev)
             
             parameter2 <- location
             parameter2.name <- char.mu
             parameter1.name <- char.sigma
             
             p.string <- paste(parameter1.name,',',
                               parameter2.name,' = ',
                               format(parameter1, nsmall = 2),',',
                               format(parameter2, nsmall = 2), sep = '')
             
          }, logistic = {
            
             range <- c(min(qlogis(prob.range[1], location = location, shape)), 
                        max(qlogis(prob.range[2], location = location, shape)))
             x <- seq(range[1], range[2], length = number.points)
             pdf.x = x
             pdf.y = SMRD:::dist.outer(x, shape, dummy = location, dlogis)
             cdf.x = x 
             cdf.y = SMRD:::dist.outer(x, shape, dummy = location, plogis)
             haz.x = x 
             haz.y = SMRD:::dist.outer(x, shape, dummy = location, dlogis)/(1-SMRD:::dist.outer(x, shape, dummy = -location,plogis))
             rel.x = x
             rel.y = 1-SMRD:::dist.outer(x, shape, dummy = location, plogis)
             
             parameter2 <- location
             parameter2.name <- char.mu
             parameter1.name <- char.sigma
             
             p.string <- paste(parameter1.name,',',
                               parameter2.name,' = ',
                               format(parameter1, nsmall = 2), ',',
                               format(parameter2, nsmall = 2), sep = '')
             
        
          }, normal = {
            
             range <- c(min(qnorm(prob.range[1], mean = location, shape)), 
                        max(qnorm(prob.range[2], mean = location, shape)))
             x <- seq(range[1], range[2], length = number.points)
             pdf.x = x 
             pdf.y = SMRD:::dist.outer(x, shape, dummy = location, dnorm)
             cdf.x = x 
             cdf.y = SMRD:::dist.outer(x, shape, dummy = location, pnorm)
             haz.x = x 
             haz.y = SMRD:::dist.outer(x, shape, dummy = location,dnorm)/(SMRD:::dist.outer(-x, shape, dummy = -location,pnorm))
             rel.x = x
             rel.y = 1-SMRD:::dist.outer(x, shape, dummy = location, pnorm)
             
             parameter2 <- location
             parameter2.name <- char.mu
             parameter1.name <- char.sigma
             
             p.string <- paste(parameter1.name,',',
                               parameter2.name,' = ',
                               format(parameter1, nsmall = 2),',',
                               format(parameter2, nsmall = 2), sep = '')

          }, lognormal = {
        
             range <- c(min(qlnorm(prob.range[1], location, shape)), 
                        max(qlnorm(prob.range[2], location, shape)))
             x <- seq(range[1], range[2], length = number.points)
             pdf.x = x 
             pdf.y = SMRD:::dist.outer(x, shape, 0, dlnorm)
             cdf.x = x 
             cdf.y = SMRD:::dist.outer(x, shape, 0, plnorm)
             haz.x = x 
             haz.y = SMRD:::dist.outer(x, shape, 0, dlnorm)/(SMRD:::dist.outer((1/x),shape, 0, plnorm))
             rel.x = x
             rel.y = 1-SMRD:::dist.outer(x, shape, 0, plnorm)
             
             parameter2 <- location
             parameter1.name <- char.sigma
             parameter2.name <- char.mu
             
             p.string <- paste(parameter1.name,',',
                               parameter2.name,' = ',
                               format(parameter1, nsmall = 2),',',
                               format(parameter2, nsmall = 2), sep = '')

          }, loglogistic = {
        
             range <- c(min(SMRD:::qloglogis(prob.range[1], location, shape)),
                        max(SMRD:::qloglogis(prob.range[2], location, shape)))
             x <- seq(range[1], range[2], length = number.points)
             pdf.x = x 
             pdf.y = SMRD:::dist.outer(x, shape, 0, SMRD:::dloglogis)
             cdf.x = x 
             cdf.y = SMRD:::dist.outer(x, shape, 0, SMRD:::ploglogis)
             haz.x = x 
             haz.y = SMRD:::dist.outer(x, shape, 0, SMRD:::dloglogis)/(1-SMRD:::dist.outer(x, shape, 0, SMRD:::ploglogis))
             rel.x = x
             rel.y = 1-SMRD:::dist.outer(x, shape, 0, SMRD:::ploglogis)
             
             parameter2 <- location
             parameter1.name <- char.sigma
             parameter2.name <- char.mu
             
             p.string <- paste(parameter1.name,',',
                               parameter2.name,' = ',
                               format(parameter1, nsmall = 2),',',
                               format(parameter2, nsmall = 2), sep = '')

          }, gamma = {
        
             range <- c(min(qgamma(prob.range[1], shape, scale = scale)), 
                        max(qgamma(prob.range[2], shape, scale = scale)))
             x <- seq(range[1], range[2], length = number.points)
             
             pdf.x = x
             pdf.y = outer(x, shape, dgamma)
             cdf.x = x 
             cdf.y = outer(x, shape, pgamma)
             haz.x = x 
             haz.y = outer(x, shape, dgamma)/(1-outer(x, shape, pgamma))
             rel.x = x
             rel.y = 1-outer(x, shape, pgamma)
             
             parameter2 <- scale
             parameter1.name <- char.kappa
             parameter2.name <- char.theta
             
             p.string <- paste(parameter1.name,',',
                               parameter2.name,' = ',
                               format(parameter1, nsmall = 2),',',
                               format(parameter2, nsmall = 2), sep = '')
             
          }, igau = {
            
             range <- c(min(qigau(prob.range[1], shape, scale = scale)), 
                        max(qigau(prob.range[2], shape, scale = scale)))
             x <- seq(range[1], range[2], length = number.points)
             pdf.x = x
             pdf.y = outer(x, shape, digau)
             cdf.x = x 
             cdf.y = outer(x, shape, pigau)
             haz.x = x 
             haz.y = outer(x, shape, digau)/(outer(x, shape, sigau))
             rel.x = x
             rel.y = outer(x, shape, sigau)
             
             parameter2 <- scale
             parameter1.name <- char.beta
             parameter2.name <- char.theta

             p.string <- paste(parameter1.name,',',
                               parameter2.name,' = ',
                               format(parameter1, nsmall = 2),',',
                               format(parameter2, nsmall = 2), sep = '')

          }, bisa = {
            
             range <- c(min(qbisa(prob.range[1], shape, scale = scale)), 
                        max(qbisa(prob.range[2], shape, scale = scale)))
             x <- seq(range[1], range[2], length = number.points)
             pdf.x = x
             pdf.y = outer(x, shape, dbisa)
             cdf.x = x 
             cdf.y = outer(x, shape, pbisa)
             haz.x = x 
             haz.y = outer(x, shape, dbisa)/(outer(x, shape,sbisa))
             rel.x = x
             rel.y = outer(x, shape, sbisa)
             
             parameter2 <- scale
             parameter1.name <- char.beta
             parameter2.name <- char.theta
             
             p.string <- paste(parameter1.name,',',
                               parameter2.name,' = ',
                               format(parameter1, nsmall = 2),',',
                               format(parameter2, nsmall = 2), sep = '')

          }, goma = {
            
             range <- c(min(qgoma(prob.range[1], shape, shape2, scale = scale)),
                        max(qgoma(prob.range[2], shape, shape2, scale = scale)))
             x <- seq(range[1], range[2], length = number.points)
             pdf.x = x
             pdf.y = SMRD:::dist.outer(x, shape, shape2, dgoma)
             cdf.x = x 
             cdf.y = SMRD:::dist.outer(x, shape, shape2, pgoma)
             haz.x = x 
             haz.y = SMRD:::dist.outer(x, shape, shape2, dgoma)/(SMRD:::dist.outer(x,shape, shape2, sgoma))
             rel.x = x
             rel.y = SMRD:::dist.outer(x, shape, shape2, sgoma)
             
             parameter2 <- shape2
             parameter3 <- scale
             parameter1.name <- char.zeta
             parameter2.name <- char.eta
             parameter3.name <- char.theta
             
             p.string <- paste(parameter1.name,',',
                               parameter2.name,',',
                               parameter3.name,' = ',
                               format(parameter1, nsmall = 2),',',
                               format(parameter2, nsmall = 2),',',
                               format(parameter3, nsmall = 2), sep = '')
             
          })
      
      pdf.df <- data.frame(x = pdf.x,
                           y = as.vector(pdf.y),
                           parameters = rep(p.string, each = number.points),
                           fun = 'pdf f(t)')
      
      cdf.df <- data.frame(x = cdf.x,
                           y = as.vector(cdf.y),
                           parameters = rep(p.string, each = number.points),
                           fun = 'cdf F(t)')
      
      haz.df <- data.frame(x = haz.x,
                           y = as.vector(haz.y),
                           parameters = rep(p.string, each = number.points),
                           fun = 'Hazard h(t)')
      
      rel.df <- data.frame(x = rel.x,
                           y = as.vector(rel.y),
                           parameters = rep(p.string, each = number.points),
                           fun = 'Survival S(t)')
      
      dist.df <- rbind(pdf.df, cdf.df, haz.df, rel.df)
      
     if (is.null(parameter2.name)) {

      } else {

      }
      
      ggplot2::ggplot(dist.df) +
  ggplot2::geom_line(ggplot2::aes(x = x,
                y = y,
                group = parameters, 
                colour = parameters,
                linetype = parameters),
            size = lwd) +
  #theme(line = element_line(size = 20)) +
  ggplot2::theme_bw(base_size = cex) +
  ggplot2::facet_wrap('fun' , nrow = 2, ncol = 2,scales = 'free_y')
 
    
    }