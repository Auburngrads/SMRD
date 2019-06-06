library(smrdfortran)
mu1 = 2/3
mu2 = 0
sd1 = sqrt(2/9)
sd2 = sqrt(2/3)
rho = 0
ah = 1
ak = 1

    number <- max(length(ah), length(ak), length(sd1), length(sd2), 
        length(mu1), length(mu2), length(rho))
    ah <-  smrdfortran:::expand.vec(ah, number)
    ak <-  smrdfortran:::expand.vec(ak, number)
    mu1 <- smrdfortran:::expand.vec(mu1, number)
    mu2 <- smrdfortran:::expand.vec(mu2, number)
    v1 <-  smrdfortran:::expand.vec(sd1 * sd1, number)
    v2 <-  smrdfortran:::expand.vec(sd2 * sd2, number)
    c12 <- smrdfortran:::expand.vec(rho * sd1 * sd2, number)
    the.frame <- data.frame(ah = ah, ak = ak, mu1 = mu1, mu2 = mu2, 
        v1 = v1, v2 = v2, c12 = c12)
    prob <- .Fortran("sbvn", 
                     as.double(the.frame$ah), 
                     as.double(the.frame$ak), 
                     as.double(the.frame$mu1), 
                     as.double(the.frame$mu2), 
                     as.double(the.frame$v1),
                     as.double(the.frame$v2), 
                     as.double(the.frame$c12), 
                     answer = double(number),
                     as.integer(number))
    
    new <- SBVN(as.double(the.frame$ah), 
                as.double(the.frame$ak), 
                as.double(the.frame$mu1), 
                as.double(the.frame$mu2), 
                as.double(the.frame$v1),
                as.double(the.frame$v2), 
                as.double(the.frame$c12), 
                double(number),
                as.integer(number),
                kprint = 0)
