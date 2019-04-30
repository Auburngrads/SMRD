urlike <-
function (tvec, 
          mut1, 
          sigmat1, 
          mut2, 
          sigmat2, 
          mur1, 
          sigmar1, 
          mur2, 
          sigmar2, 
          rho,
          kprint = 0) 
{
    
    zout <- SURLIKE(t = as.double(tvec), 
                    nt = as.integer(length(tvec)), 
                    mut1 = as.double(mut1), 
                    sigmat1 = as.double(sigmat1), 
                    mut2 = as.double(mut2),
                    sigmat2 = as.double(sigmat2), 
                    mur1 = as.double(mur1), 
                    sigmar1 = as.double(sigmar1),
                    mur2 = as.double(mur2), 
                    sigmar2 = as.double(sigmar2), 
                    rho = as.double(rho), 
                    answer = double(length(tvec)),
                    kprint = as.integer(kprimt))
    
    return(zout$answer)
    
}
