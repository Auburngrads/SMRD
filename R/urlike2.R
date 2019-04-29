urlike2 <-
function (tvec, mut1, sigmat1, mut2, sigmat2, mur1, sigmar1, 
    mur2, sigmar2, rho, ier, xfactor = 4.5) 
{
    tlog <- log(tvec)
    r1lower <- mur1 - xfactor * sigmar1
    r1upper <- mur1 + xfactor * sigmar1
    urlike <- rep(NA, length(tlog))
    for (i in 1:length(tlog)) {
        answer1 <- integrate(urlikeint, lower = r1lower, upper = mur1, 
            tlog = tlog[i], mut1 = mut1, sigmat1 = sigmat1, mut2 = mut2, 
            sigmat2 = sigmat2, mur1 = mur1, sigmar1 = sigmar1, 
            mur2 = mur2, sigmar2 = sigmar2, rho = rho, abs.tol = 0)
        answer2 <- integrate(urlikeint, lower = mur1, upper = r1upper, 
            tlog = tlog[i], mut1 = mut1, sigmat1 = sigmat1, mut2 = mut2, 
            sigmat2 = sigmat2, mur1 = mur1, sigmar1 = sigmar1, 
            mur2 = mur2, sigmar2 = sigmar2, rho = rho, abs.tol = 0)
        print(answer1)
        print(answer2)
        urlike[i] <- log(answer1$integral + answer2$integral)
    }
    return(urlike)
}
