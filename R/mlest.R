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
          genforce = F,...)
{
    simple.distribution <- (numdist(distribution) <= 10 && numdist(distribution) >=
        0) || numdist(distribution) == 14
    
    simple.regr.model <- (is.atomic(explan.vars) || is.null(explan.vars)) &&
        relationship.parameters == 1
    
    simple.model <- simple.distribution && simple.regr.model
    
    if (simple.model && !genforce) {
      
        return(star2.mlest(data.ld = data.ld, 
                           distribution = distribution,
                           explan.vars = explan.vars, 
                           gamthr = gamthr, 
                           theta.start = theta.start,
                           parameter.fixed = parameter.fixed, 
                           intercept = intercept,
                           kprint = kprint,
                           debug1 = debug1,...))
    } else {
  
        return(gmlest(data.ld, 
                      distribution, 
                      theta.start = theta.start,
                      parameter.fixed = parameter.fixed, 
                      kprint = kprint,
                      explan.vars = explan.vars,...))
    }
}
