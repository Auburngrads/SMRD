svecml1grow <-
function (times, 
          a2.init, 
          a2.limit, 
          rate, 
          rate.factor) 
{
    number.times <- length(times)
    rate.factor <- expand.vec(rate.factor, number.times)
    
    zout <- MLMOD1(as.double(times), 
                   as.integer(number.times), 
                   as.double(a2.init), 
                   as.double(a2.limit),
                   as.double(rate), 
                   as.double(rate.factor),
                   a2 = double(number.times))
    
    return(zout$a2)
}
