library(SMRD)
z = seq(0.1,0.9,0.1)
distribution = 'sev' 

  idist <- SMRD:::numdist(distribution)
  if (idist < 1 || idist > 8) 
    stop("Distribution must be sev, normal, logistic, or lev or corresponding log location-scale")
    length.z <- length(z)
old <- .Fortran("wqm_phiall", phib = double(length.z), phibm = double(length.z), 
                 phis = double(length.z), phip = double(length.z), 
                 as.double(z), as.integer(idist))

new <- SMRD2:::wqmsphiall(double(length.z), 
                                 double(length.z), 
                                 double(length.z), 
                                 double(length.z), 
                                 as.double(z), 
                                 n = length.z,
                                 as.integer(idist))

matrix(c(old[1:length(z)], new[1:length(z)]), 
       ncol = 2, 
       nrow = length(z),
       byrow = F,
       dimnames = list(names(old[1:length(z)]), c('old','new')))
