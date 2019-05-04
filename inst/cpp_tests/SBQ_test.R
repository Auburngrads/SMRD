library(SMRD)
ndist1 = 2
ndist2 = 2
alpha = .5
bds = c(5,10)
beta0 = 30.27241
beta1 = -5.100121
stress = 270
sigma = 0.2894549
mugamma = 5.365834
sdgamma = 0.03140004
kprint = 1

    bd1 <- bds[1]
    bd2 <- bds[2]
    zout <- .Fortran("sbq", as.integer(ndist1), as.integer(ndist2), 
        as.double(stress), as.double(alpha), as.double(beta0), 
        as.double(beta1), as.double(sigma), as.double(mugamma), 
        as.double(sdgamma), as.double(bd1), as.double(bd2), quan = double(1))

    new <- SMRD2::SBQ(as.integer(ndist1), as.integer(ndist2), 
        as.double(stress), as.double(alpha), as.double(beta0), 
        as.double(beta1), as.double(sigma), as.double(mugamma), 
        as.double(sdgamma), as.double(bd1), as.double(bd2), 
        quan = double(1), as.integer(kprint))

