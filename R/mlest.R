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
#' @param embedded
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
          embedded = F,...)
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
    
    if(!embedded) mlest.out = print.mlest(mlest.out, printem = F)
    
return(mlest.out)
    
}
