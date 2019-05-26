beta0.2.quan <-
function (ndist1, 
          ndist2, 
          alpha, 
          bds, 
          beta0, 
          beta1, 
          stress, 
          sigma,
          mugamma, 
          sdgamma,
          kprint = 0) 
{
    bd1 <- bds[1]
    bd2 <- bds[2]
    zout <- SBQ(as.integer(ndist1), 
                as.integer(ndist2), 
                as.double(stress), 
                as.double(alpha), 
                as.double(beta0), 
                as.double(beta1), 
                as.double(sigma), 
                as.double(mugamma), 
                as.double(sdgamma), 
                as.double(bd1), 
                as.double(bd2), 
                quan = double(1),
                as.integer(kprint))
    
    return(zout$quan)
    
}
