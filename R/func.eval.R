func.eval <-
function (theta.short) 
{
    profile.on.pos <- get(envir = .frame0,  "profile.on.pos")
    theta.start <- get(envir = .frame0,  "theta.start")
    theta.start[profile.on.pos] <- theta.short
    assign(envir = .frame0, inherits = !TRUE,"theta.hold", value = theta.start)
    theta.opt <- theta.start[Uminus(profile.on.pos)]
 
    cmle <- wqm.nlmin(const.log.like, theta.opt, max.fcal = 1000, 
        max.iter = 900)
    theta.start[Uminus(profile.on.pos)] <- cmle$x
   
    assign(envir = .frame0, inherits = !TRUE,"theta.start", theta.start)
    return(const.log.like(cmle$x))
}
