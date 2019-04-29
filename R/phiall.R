phiall <-
function (z, distribution) 
{
    idist <- numdist(distribution)
    
    if (idist < 1 || idist > 8) {
      
        stop("Distribution must be sev, normal, logistic, or lev or corresponding log location-scale")
      
    }
    
    length.z <- length(z)
    
    zout <- .Fortran("wqmsphiall", 
                     phib = double(length.z), 
                     phibm = double(length.z), 
                     phis = double(length.z), 
                     phip = double(length.z), 
                     n = as.integer(length.z),
                     as.double(z), 
                     as.integer(idist))
    
    answer <- cbind(z = z, 
                    phib = zout$phib, 
                    phibm = zout$phibm, 
                    phis = zout$phis, 
                    phip = zout$phip)
    
    return(answer)
}
