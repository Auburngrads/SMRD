z.eval <-
function (gmle.out, profile.setup.out, xlim, ylim, profile.on,
    size = c(10, 10),debug1= 1, alt.max = F)
{
    is.min <- function (x) { x == min(x) }
    is.maxele <- function (x) { seq(1:length(x))[is.min(.Uminus(x))][1] }
    profile.on.pos <- profile.setup.out$profile.on.pos
    h.theta.hat <- profile.setup.out$h.theta.hat
    assign(envir = .frame0,  inherits = TRUE,"profile.on.pos", value = profile.on.pos)
    assign(envir = .frame0,  inherits = TRUE,"profile.on", value = profile.on)
    log.scale <- gmle.out$max.log.like
    xvec <- seq(xlim[1], xlim[2], length = 2 * size[1] +
        1)
    yvec <- seq(ylim[1], ylim[2], length = 2 * size[2] +
        1)
    z <- matrix(rep(0, length(xvec) * length(yvec)), ncol = length(xvec),
        nrow = length(yvec))
    jmid.start <- mid.start(h.theta.hat[profile.on.pos[1]], xlim,
        size[1])
    imid.start <- mid.start(h.theta.hat[profile.on.pos[2]], ylim,
        size[2])
    index.list <- profile.list(size[1], jmid.start)
   
    kount <- 0
    increment <- ceiling(length(index.list)/15)
    for (j in index.list) {
        kount <- kount + 1
        if (j == jmid.start) {
            assign(envir = .frame0,  inherits = TRUE,"theta.mid.last", h.theta.hat)
            imid <- imid.start
        }
        z[j, ] <- inner.profile(j, xvec, yvec, imid, jmid.start)
        imid <- is.maxele(z[j, ])
       
    }
    if (alt.max) {
        rel.likelihood <- exp(z - max(z))
  } else {
        rel.likelihood <- exp(z - log.scale)
    }
    return(list(x = xvec, y = yvec, z = rel.likelihood, zlog = z,
        xlab = profile.setup.out$profile.name[1], ylab = profile.setup.out$profile.name[2],
        distribution = gmle.out$model$distribution, number.parameters = length(gmle.out$est.out$x),
        form = gmle.out$model$form, subtitle = get.data.title(gmle.out$data.ld)))
}
