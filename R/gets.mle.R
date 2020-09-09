#' Title
#'
#' @param data.ld 
#' @param distribution 
#' @param theta.start 
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' lzbearing.ld <- frame.to.ld(lzbearing, 
#'                             response.column = 1)
#'                             
#' bear.gets.sev.gmle.out <- gets.mle(lzbearing.ld, 
#'                                    distribution = "sev")
#'                                    
#' Fan.ld <- frame.to.ld(fan,
#'                       response.column = 1, 
#'                       censor.column = 2, 
#'                       case.weight.column = 3,
#'                       time.units = "Hours")
#'                     
#' gets.mle(small.interval(Fan.ld, delta = 0.01),
#'          distribution = "sev")
#' 
#' gets.mle(small.interval(Fan.ld, delta = 0.01),
#'          distribution = "normal")
#' 
#' AlloyC.ld <- frame.to.ld(alloyc,
#'                          response.column = c(1,2),
#'                          censor.column = 3, 
#'                          case.weight.column = 4,
#'                          data.title = "Alloy C", 
#'                          time.units = "ksi")
#' 
#' gets.mle(AlloyC.ld, distribution = "sev")
#' gets.mle(AlloyC.ld, distribution = "normal")
#' 
#' }
gets.mle <-
function (data.ld, 
          distribution, 
          theta.start = NULL) 
{
    options(digits = 5)
    f.origparam <- function(thetatran, model) {
        logtp1 <- thetatran[1]
        logtp2 <- thetatran[2]
        sigma <- thetatran[3]
        logwp2 <- qgets(model$p2, 0, sigma, 1, distribution = model$sub.distribution)
        logwp1 <- qgets(model$p1, 0, sigma, 1, distribution = model$sub.distribution)
        alpha <- (logwp2 * logtp1 - logwp1 * logtp2)/(logwp2 - logwp1)
        varzeta <- (logtp2 - logtp1)/(logwp2 - logwp1)
        thetaorig <- c(alpha, sigma, varzeta)
        names(thetaorig) <- model$orig.param.names
        return(thetaorig)
    }
    f.interpparam <- function(thetaorig) {
        gamma <- thetaorig[1] - thetaorig[3]/thetaorig[2]
        mu <- logb(abs(thetaorig[3]/thetaorig[2]))
        sigma <- thetaorig[2]
        thetainterp <- c(gamma, mu, sigma)
        names(thetainterp) <- c("gamma", "mu", "sigma")
        return(thetainterp)
    }
    assign(envir = .frame0, inherits = !TRUE,"iter.count", 0 )
    probs <- cdfest(data.ld)$prob
    p1 <- min(probs[probs > 0])/2
    p2 <- 0.9 * max(probs)
    orig.param.names <- c("alpha", "sigma", "varzeta")
    t.param.names <- c("logtp1", "logtp2", "sigma")
    model <- list(p1 = p1, 
                  p2 = p2, 
                  sub.distribution = distribution, 
                  distribution = paste(generic.distribution(distribution), "gets", sep = ""))
    
    if (is.null(theta.start)) {
        
        ls.gmle.out <- mlest(data.ld, distribution = distribution)
        alpha <- ls.gmle.out$theta.hat[1]
        varzeta <- ls.gmle.out$theta.hat[2]
        sigma <- 0.001
        logtp1 <- qgets(p1, alpha, sigma, varzeta, distribution = model$sub.distribution)
        logtp2 <- qgets(p2, alpha, sigma, varzeta, distribution = model$sub.distribution)
        theta.start <- c(logtp1, logtp2, sigma)
        
    }
    
    gmle.out <- gmle(data.ld = data.ld, 
                     log.like = gets.log.like, 
                     theta.start = theta.start, 
                     model = model, 
                     f.origparam = f.origparam, 
                     t.param.names = t.param.names, 
                     orig.param.names = orig.param.names)
    
    thetainterp <- f.interpparam(gmle.out$origparam)
    gmle.out$thetainterp <- thetainterp
    
    return(gmle.out)
    
}
