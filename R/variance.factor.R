variance.factor <- 
  function(distribution = NULL, type = 'quantile', 
           quantile.of.interest = NULL, proportion.failing = NULL,
           number.points = 200,...) {

  if(!(tolower(type)%in%c('quantile', 'hazard'))) stop("\nArgument type must be either 'quantile' or 'hazard'")

  if(tolower(type)=='hazard') {
    
    hazard.var.fact(distribution = distribution, 
                                           number.points = number.points)
} else {
    
  if(is.null(proportion.failing) & is.null(quantile.of.interest)) {
    
    quantile.var.fact(x = distribution, number.points = number.points)
    
} else {
    
    if(is.null(proportion.failing) | is.null(quantile.of.interest)) stop('\nQuantile.of.interest and proportion.failing must be specified')

  single.quantile.var.fact(quant.of.interest = quantile.of.interest, 
                           proportion.failing = proportion.failing,
                           distribution = distribution)    
}}
                                                                                  
  
  
  
  
  
}