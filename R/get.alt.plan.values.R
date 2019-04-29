alt.plan.values <-
function (distribution, relationship, slope, probs, accelvar, 
    censor.time, beta, sigma, time.units = "Time", accelvar.units, 
    power = NULL, use.conditions = NULL) 
{
  
    if (length(probs) == 1)  {
        
    get.alt.plan.values.from.slope.and.point(distribution = distribution, relationship = relationship, 
                                             slope = slope, probs = probs, accelvar = accelvar, 
                                             censor.time = censor.time, beta = beta, sigma = sigma, 
                                             time.units = time.units, accelvar.units = accelvar.units, 
                                             power = power, use.conditions = use.conditions)
    
  } else {
    
    get.alt.plan.values.from.two.points(distribution = distribution, relationship = relationship, 
                                        probs = probs, accelvar = accelvar, 
                                        censor.time = censor.time, beta = beta, sigma = sigma, 
                                        time.units = time.units, accelvar.units = accelvar.units, 
                                        power = power)
      
  }
}
