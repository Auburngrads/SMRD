inner.profile <-
function (j, xvec, yvec, imid, jmid)
{
    size2 <- length(yvec)%/%2
    zz <- rep(0, length(yvec))
    index.list <- profile.list(size2, imid)
   
    assign(envir = .frame0, inherits = !TRUE,"theta.mid.now", get(envir = .frame0,  "theta.mid.last"))
    for (i in index.list) {
        if (i == imid)
            assign(envir = .frame0, inherits = !TRUE,"theta.start", get(envir = .frame0,  "theta.mid.now"))
        zz[i] <- Uminus(func.eval(c(xvec[j], yvec[i])))
        if (i == imid)
            assign(envir = .frame0, inherits = !TRUE,"theta.mid.last", get(envir = .frame0,  "theta.start"))

    }
    return(zz)
}
