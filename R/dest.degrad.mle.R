dest.degrad.mle <-
function (data.ddd, distribution, transformation.response, transformation.x,
    transformation.time, group.var = 1:ncol(xmat(data.ddd)),
   debug1= map.SMRDDebugLevel() >= 4, theta.start = NULL,
    power = NULL)
{
  `do.list<-` <-
    function (data.ld, value)
    {
      attr(data.ld, "do.list") <- value
      return(data.ld)
    }

  ADDTZeroRepsFilter <-
    function (data.ddd, group.var = 1:ncol(the.xmat))
    {
      the.times <- times(data.ddd)
      zero.times <- the.times == 0
      if (!any(zero.times))
        return(data.ddd)
      the.xmat <- xmat(data.ddd)
      the.markers <- apply(the.xmat[, group.var, drop = F], 1,
                           paste, sep = "", collapse = ";")
      the.unique.markers <- unique(the.markers)
      the.first.zeros <- the.markers == the.unique.markers[1] &
        zero.times
      if (!any(the.first.zeros))
        return(data.ddd)
      first.response <-Response(data.ddd)[the.first.zeros]
      reps.found <- rep(F, length = length(the.unique.markers) -
                          1)
      for (i in 2:length(the.unique.markers)) {
        the.current.zeros <- the.markers == the.unique.markers[i] &
          zero.times
        current.response <-Response(data.ddd)[the.current.zeros]
        if (length(current.response) > 0 && length(first.response) >
            0 && (length(first.response) == length(current.response) &&
                  all(sort(first.response) == sort(current.response))))
          reps.found[i - 1] <- T
      }
      if (F && !any(reps.found))
        warning("There are 0 times, but no replication.\n  No filtering will be done.\n")
      if (any(reps.found) && !all(reps.found))
        warning(paste("Some reps at time 0 found in the data set.\nNo filtering will be done.\n"))
      if (all(reps.found)) {
        cat("\nFiltering 0-time replicates for analysis.\n")
        the.filtered.zeros <- the.markers != the.unique.markers[1] &
          zero.times
        the.return <- data.ddd[!the.filtered.zeros, ]
        attr(the.return, "did.filter") <- T
        return(the.return)
      }
      attr(data.ddd, "did.filter") <- F
      return(data.ddd)
    }

    do.list <- function (data.d) { return(attr(data.d, "do.list")) }
    data.ddd <- ADDTZeroRepsFilter(data.ddd, group.var = group.var)
    transformation.x <- set.relationship.power(transformation.x,
        power)
    transformation.x <- fix.inverse.relationship(transformation.x)
    x.tran <- xmat(data.ddd)[, group.var, drop = F]
    if (length(transformation.x) != length(group.var))
        stop("length(transformation.x)!=group.var")
    for (i in 1:length(transformation.x)) {
        x.tran[, i] <- f.relationship(x.tran[, i], subscript.relationship(transformation.x,
            i))
    }
    trans.data.ddd <- data.ddd
   Response(trans.data.ddd) <- as.matrix(f.relationship(Response(trans.data.ddd),
        transformation.response))
    xmat(trans.data.ddd) <- as.matrix(x.tran)
    times(trans.data.ddd) <- f.relationship(times(trans.data.ddd),
        transformation.time)
    do.list(trans.data.ddd) <- get.x.markers(data.ddd, group.var = group.var,
        long = T, include.complete = T)
    complete.list(trans.data.ddd) <- complete.list(do.list(trans.data.ddd))
    assign(envir = .frame0,  inherits = TRUE,"tmp.trans.data.ddd", trans.data.ddd)
    f.origparam <- .f.ADDT.origparam
    f.stableparam <- .f.ADDT.stableparam
    assign(envir = .frame0,  inherits = TRUE,"iter.count", 0 )
    orig.param.names <- c("beta0", "beta1", paste("beta", seq(2,
        length(transformation.x) + 1), sep = ""), "sigma")
    t.param.names <- c("gamma0", "gamma1", paste("gamma", seq(2,
        length(transformation.x) + 1), sep = ""), "logsigma")
    the.xmat <- xmat(trans.data.ddd)
    model <- list(tbar = mean(times(trans.data.ddd)), xbar = apply(the.xmat,
        2, mean), distribution = distribution, stable.param.names = t.param.names,
        orig.param.names = orig.param.names, param.names = orig.param.names,
        transformation.response = transformation.response, transformation.x = transformation.x,
        transformation.time = transformation.time)
    oldClass(model) <- "ADDT.model"
    if (is.null(theta.start)) {
        theta.start.orig <- two.stage.dest.degrad(trans.data.ddd,
            distribution = distribution)
        theta.start <- f.stableparam(theta.start.orig, model)
    }
    if (debug1) {
        gmle.out.dummy <- list(model = model, origparam = theta.start.orig)
        the.stresses <- xmat(data.ddd)[!duplicated(complete.list(trans.data.ddd)),
            , drop = F]
        sub.model <- get.sub.model.dest.degrad.mle.out(gmle.out.dummy,
            stresses = the.stresses)
        slope <- sub.model$slope
        intercept <- sub.model$intercept
        for (i in 1:length(slope)) {
            cat("Start slope at", paste(the.stresses[i, ], collapse = ","),
                "Intercept=", format(intercept[i]), "slope=",
                format(slope[i]), "in dest.degrad.mle\n")
        }
    }
    for (ml.try in 1:10) {
        if (ml.try > 1)
            cat(paste("**************** Restart", ml.try, "*********************\n"))
        theta.start.now <- theta.start
        theta.start.now[3] <- theta.start.now[3]/(1 + (ml.try -
            1)/10)
        gmle.out <- gmle(data.ld = trans.data.ddd, log.like = dest.degrad.log.like,
            theta.start = theta.start.now, model = model, f.origparam = f.origparam,
            t.param.names = t.param.names, orig.param.names = orig.param.names,
           debug1= debug1)
        if (debug1) {
            cat("Start values=", paste(format(theta.start.now),
                collapse = ","), "\n")
            cat("Final values=", paste(format(gmle.out$t.param),
                collapse = ","), "\n")
        }
        grad.problem <- any(abs(gmle.out$grad) > 0.1)
        if (!is.na(grad.problem) && !grad.problem)
            break
        print(gmle.out)
    }
    gmle.out$thetainterp <- gmle.out$origparam
    gmle.out$trans.data.ddd <- trans.data.ddd
    gmle.out$x.names <- colnames(x.tran)
    gmle.out$group.var <- group.var
    gmle.out$data.ld <- data.ddd
    gmle.out$kodet <- c(1, 1, rep(2, length(group.var)), 2)
    oldClass(gmle.out) <- c("dest.degrad.mle.out", "gmle.out")
    MysetOldClass(attr(gmle.out, "class"))
    return(gmle.out)
}
