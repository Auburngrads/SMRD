y.eval <-
function (gmle.out, profile.setup.out, xlim = NULL, profile.on,
    size = 10,debug1= 1, save.parameter.vectors = F)
{
    profile.on.pos <- profile.setup.out$profile.on.pos
    assign(envir = .frame0,  inherits = TRUE,"profile.on.pos", value = profile.on.pos)
    assign(envir = .frame0,  inherits = TRUE,"profile.on", value = profile.on)
    h.theta.hat <- profile.setup.out$h.theta.hat
    assign(envir = .frame0,  inherits = TRUE,"h.theta.hat", value = h.theta.hat)
    log.scale <- gmle.out$max.log.like
    xvec <- seq(xlim[1], xlim[2], length = 2 * size + 1)
    y <- rep(0, length(xvec))
    if (save.parameter.vectors) {
        parameter.matrix <- matrix(0, ncol = length(gmle.out$t.param),
            nrow = length(xvec))
  } else {
        parameter.matrix <- NULL
    }
    jmid.start <- mid.start(h.theta.hat[profile.on.pos], xlim,
        size)
    index.list <- profile.list(size, jmid.start)
   
    kount <- 0
    increment <- ceiling(length(index.list)/15)
    for (j in index.list) {
        kount <- kount + 1
        if (j == jmid.start)
            assign(envir = .frame0,  inherits = TRUE,"theta.start", h.theta.hat)
        y[j] <- .Uminus(func.eval(xvec[j]))
        if (save.parameter.vectors) {
            parameter.matrix[j, ] <- get(envir = .frame0,  "theta.start")
        }
      
    }
    return(list(x = xvec, y = exp(y - log.scale), xlab = profile.setup.out$profile.name,
        distribution = gmle.out$model$distribution, number.parameters = length(gmle.out$est.out$x),
        form = gmle.out$model$form, parameter.matrix = parameter.matrix,
        subtitle = get.data.title(gmle.out$data.ld)))
}
