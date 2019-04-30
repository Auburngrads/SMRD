library(smrdfortran)
timevec = rexp(10)
mu = 0
sigma = 1
distribution = 'weibull' # issues with exponential

old <- .Fortran("postpr", as.double(timevec), as.integer(length(timevec)), 
                as.double(mu), as.double(sigma), as.integer(length(mu)), 
                as.integer(smrdfortran:::numdist(distribution)), pdf = double(length(timevec)), 
                cdf = double(length(timevec)))

new <- SMRD2::POSTPR(as.double(timevec), as.integer(length(timevec)), 
                        as.double(mu), as.double(sigma), as.integer(length(mu)), 
                        as.integer(smrdfortran:::numdist(distribution)), pdf = double(length(timevec)), 
                        cdf = double(length(timevec)))

old$cdf - new$cdf
old$pdf - new$pdf
