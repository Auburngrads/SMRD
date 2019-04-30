cprobur <-
function (tvec, 
          r1log, 
          mut2, 
          sigmat2, 
          mur1, 
          sigmar1, 
          mur2, 
          sigmar2,
          rho) 
{
    
    zout <- SSFT2GR1(t = as.double(tvec), 
                     nt = as.integer(length(tvec)), 
                     as.double(r1log), 
                     mut2 = as.double(mut2), 
                     sigmat2 = as.double(sigmat2), 
                     mur1 = as.double(mur1), 
                     sigmar1 = as.double(sigmar1), 
                     mur2 = as.double(mur2), 
                     sigmar2 = as.double(sigmar2), 
                     rho = as.double(rho), 
                     answer = double(length(tvec)),
                     ier = integer(1),
                     kprint = kprint)
    
    return(zout$answer)
    
}
