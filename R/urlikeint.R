urlikeint <-
function (r1log, tlog, mut1, sigmat1, mut2, sigmat2, mur1, sigmar1, 
    mur2, sigmar2, rho) 
{
    mut1gr1 <- mut1 - r1log
    ztigr1 <- (tlog - mut1gr1)/sigmat1
    zr1 <- (r1log - mur1)/sigmar1
    tmp1 <- cprobur(tlog, r1log, mut2, sigmat2, mur1, sigmar1, 
        mur2, sigmar2, rho)
    tmp2 <- dnorm(ztigr1)/sigmat1
    tmp3 <- dnorm(zr1)/sigmar1
    return(tmp1 * tmp2 * tmp3)
}
