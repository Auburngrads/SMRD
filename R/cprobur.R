cprobur <-
function (tvec, r1log, mut2, sigmat2, mur1, sigmar1, mur2, sigmar2, 
    rho) 
{
    zout <- .Fortran("ssft2gr1", tvec = as.double(tvec), nt = as.integer(length(tvec)), 
        mut1 = as.double(r1log), mut2 = as.double(mut2), sigmat2 = as.double(sigmat2), 
        mur1 = as.double(mur1), sigmar1 = as.double(sigmar1), 
        mur2 = as.double(mur2), sigmar2 = as.double(sigmar2), 
        rho = as.double(rho), answer = double(length(tvec)))
    return(zout$answer)
}
