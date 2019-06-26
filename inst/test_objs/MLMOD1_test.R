library(smrdfortran)
times = rexp(10, 0.02)
a2.init = 1
a2.limit = 200
rate = 0.5
rate.factor = 2
number.times <- length(times)
rate.factor <- smrdfortran:::expand.vec(rate.factor, number.times)
old <- .Fortran("mlmod1", as.double(times), as.integer(number.times), 
                 as.double(a2.init), as.double(a2.limit), as.double(rate), 
                 as.double(rate.factor), a2 = double(number.times))

new <- SMRD::MLMOD1(as.double(times), 
                     as.integer(number.times), 
                     as.double(a2.init), 
                     as.double(a2.limit),
                     as.double(rate), 
                     as.double(rate.factor),
                     a2 = double(number.times))
