const.log.like <-
function (theta.opt) 
{
    log.like <- get(envir = .frame0,  "log.like")
    theta.hold <- get(envir = .frame0,  "theta.hold")
    profile.on <- get(envir = .frame0,  "profile.on")
    profile.stable.parameters <- get(envir = .frame0,  "profile.stable.parameters")
    profile.on.pos <- get(envir = .frame0,  "profile.on.pos")
    theta <- theta.hold
    theta[Uminus(profile.on.pos)] <- theta.opt
    theta.now <- profile.stable.parameters(theta, profile.on)
    return(log.like(theta.now))
}
