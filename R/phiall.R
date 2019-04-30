phiall <-
function (z, distribution) 
{
    idist <- numdist(distribution)
    
    if (idist < 1 || idist > 8) {
      
        stop("Distribution must be sev, normal, logistic, or lev or corresponding log location-scale")
      
    }
    
    length.z <- length(z)
    
    zout <- WQMSPHIALL(double(length.z), 
                       double(length.z), 
                       double(length.z), 
                       double(length.z), 
                       as.double(z), 
                       n = length.z,
                       as.integer(idist))
    
    answer <- cbind(z = z, 
                    phib = zout$phib, 
                    phibm = zout$phibm, 
                    phis = zout$phis, 
                    phip = zout$phip)
    
    return(answer)
}
