sim.type.two <-
function (n, 
          r, 
          distribution, 
          number.sim = 100)
{
    results <- SingleDistSim(number.sim = number.sim, 
                             distribution = distribution,
                             sample.size = n, 
                             censor.type = "Type 2",
                             fail.number = r)
    
    attr(results, "sample.size") <- n
    attr(results, "number.fail") <- r
    attr(results, "date") <- date()
    attr(results, "distribution") <- distribution
    oldClass(results) <- "simulate.type.two.out"
    return(results)
    
}
