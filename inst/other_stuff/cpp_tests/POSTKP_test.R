library(smrdfortran)
korder = 2
nsamsize = 10
timevec = 5
mu = 0 
sigma = 1
distribution = 'lognormal' 

old <- .Fortran("postkp", as.integer(korder), as.integer(nsamsize), 
                as.double(timevec), as.integer(length(timevec)), as.double(mu), 
                as.double(sigma), as.integer(length(mu)), as.integer(smrdfortran:::numdist(distribution)), 
                pdf = double(length(timevec)), cdf = double(length(timevec)))

new <- SMRD::postkp(as.integer(korder), as.integer(nsamsize), 
                        as.double(timevec), as.integer(length(timevec)), as.double(mu), 
                        as.double(sigma), as.integer(length(mu)), as.integer(smrdfortran:::numdist(distribution)), 
                        pdf = double(length(timevec)), cdf = double(length(timevec)))

old$pdf - new$pdf
old$cdf - new$cdf
