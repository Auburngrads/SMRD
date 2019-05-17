
FillRegion <-
function (gmle.out, iter = 2000, conf.level = (GetSMRDDefault("SMRD.ConfLevel") +
    1)/100, prt = T, lower = NULL, upper = NULL, cull = 1, nbound = 5,
    simult = F)
{
  random.direction <-
    function (n)
    {
      x <- runif(n, -1, 1)
      while (sum(x^2) > 1 || sum(x^2) == 0) {
        x <- runif(n, -1, 1)
      }
      dir.vector <- x/sqrt(sum(x^2))
      return(dir.vector)
    }
    assign(envir = .frame0,  inherits = TRUE,"iter.count", 0 )
    start.date <- date()
    theta.hat <- gmle.out$est.out$x
    dim <- length(theta.hat)
    label <- names(gmle.out$t.param)
    l.bounds <- NULL
    u.bounds <- NULL
    original.type <- NULL
    jcrname <- paste(deparse(substitute(gmle.out)), "jcr", sep = ".")
    if (exists(jcrname)) {
        jcr <- get(envir = .frame0, jcrname)
        if (gmle.out$date != jcr$date) {
            the.string <- paste("\nThe gmle create date=", gmle.out$date,
                "\ndoes not agree with the jcr$date=", jcr$date,
                "\n")
            browser()
            cat(the.string)
            warning("Error")
        }
        jcr.size <- jcr$jcr.size
        l.bounds <- jcr$l.bounds
        u.bounds <- jcr$u.bounds
        start.theta <- jcr$conf.reg[jcr.size, 1:dim]
        original.type <- jcr$type
        conf.level <- jcr$conf.level
  } else {
        start.theta <- theta.hat
    }
   debug1<- 1
    log.like <- gmle.out$log.like
    std.error <- gmle.out$t.standard.error
    assign(envir = .frame0,  inherits = TRUE,"model", value = gmle.out$model)
    assign(envir = .frame0,  inherits = TRUE,"debug1", value = debug1)
    assign(envir = .frame0,  inherits = TRUE,"log.like", value = gmle.out$log.like)
    assign(envir = .frame0,  inherits = TRUE,"data.ld", value = gmle.out$data.ld)
    sim.bound <- log.like(theta.hat) + 0.5 * qchisq(conf.level,
        dim)
    pt.bound <- log.like(theta.hat) + 0.5 * qchisq(conf.level,
        1)
    size <- trunc(iter/cull)
    if (is.null(original.type)) {
        if (simult == F) {
            type <- "pointwise"
      } else {
            type <- "simultaneous"
        }
  } else {
        type <- original.type
    }
    if (type == "pointwise") {
        sim <- F
        dd <- matrix(0, size, dim + 1, dimnames = list(NULL,
            c(label, "-loglikelihood")))
        region.bound <- pt.bound
  } else {
        sim <- T
        dd <- matrix(0, size, dim + 2, dimnames = list(NULL,
            c(label, "sim/pt", "-loglikelihood")))
        region.bound <- sim.bound
    }
    ncull <- 0
    np <- 0
    npts <- 1
    theta <- start.theta
    if (is.null(u.bounds)) {
        if (is.null(upper))
            upper <- theta.hat + nbound * std.error
  } else {
        upper <- u.bounds
    }
    if (is.null(l.bounds)) {
        if (is.null(lower))
            lower <- theta.hat - nbound * std.error
  } else {
        lower <- l.bounds
    }
    while (npts <= iter) {
        d <- random.direction(dim)
        t2 <- (upper - theta)/d
        t1 <- (lower - theta)/d
        r2 <- min(t1[t1 > 0], t2[t2 > 0])
        r1 <- max(t1[t1 < 0], t2[t2 < 0])
        r <- runif(1, r1, r2)
        repeat {
            theta1 <- theta + r * d
            lt <- log.like(theta1)
            if (lt < region.bound)
                break
            if (r < 0)
                r <- runif(1, r, r2)
            else r <- runif(1, r1, r)
        }
        ncull <- ncull + 1
        if (ncull == cull) {
            np <- np + 1
            if (!sim) {
                dd[np, ] <- c(theta1, lt)
          } else {
                if (lt < pt.bound)
                  dd[np, ] <- c(theta1, 1, lt)
                else dd[np, ] <- c(theta1, 0, lt)
            }
            ncull <- 0
        }
        if (prt == T) {
            if (npts%%round(0.1 * iter) == 0 || npts == iter) {
                cat(round((100 * npts)/iter), "% of the points generated \n")
            }
        }
        theta <- theta1
        npts <- npts + 1
    }
    end.date <- date()
    lb <- rep(F, dim)
    ub <- rep(F, dim)
    for (i in 1:dim) {
        bd <- range(dd[, i])
        near.lower <- lower[i] + 0.05 * (upper[i] - lower[i])
        near.upper <- upper[i] - 0.05 * (upper[i] - lower[i])
        if (bd[1] < near.lower)
            lb[i] <- T
        if (bd[2] > near.upper)
            ub[i] <- T
    }
    if (!is.null(gmle.out$jcrname) && exists(jcrname)) {
        dd.old <- jcr$conf.reg
        size.time <- jcr$jcr.size.time
        dd <- rbind(dd.old, dd)
        size.time.new <- paste(size, start.date, end.date)
        jcr.size.time <- c(size.time, size.time.new)
        jcr.size <- jcr$jcr.size + size
        lower.2close <- jcr$lower.2close
        lower.2close <- rbind(lower.2close, lb, deparse.level = 0)
        upper.2close <- jcr$upper.2close
        upper.2close <- rbind(upper.2close, ub, deparse.level = 0)
  } else {
        jcr.size.time <- paste(size, start.date, end.date)
        jcr.size <- size
        gmle.out$jcrname <- jcrname
        lower.2close <- matrix(lb, nrow = 1, dimnames = list(NULL,
            label))
        upper.2close <- matrix(ub, nrow = 1, dimnames = list(NULL,
            label))
    }
    gmle.out$jcrtime <- end.date
    return.list <- list(conf.reg = dd, jcr.size.time = jcr.size.time,
        jcr.size = jcr.size, type = type, l.bounds = lower, u.bounds = upper,
        upper.2close = upper.2close, lower.2close = lower.2close,
        conf.level = conf.level, gmle.out = gmle.out, date = gmle.out$date)
    oldClass(return.list) <- "FillRegion.out"
    assign(envir = .frame0,  inherits = TRUE,jcrname, value = return.list)
    return(gmle.out)
}
