ADDTsim <-
function (ADDT.test.plan, ADDT.plan.values, number.sim = 3, plotem = F,
    xlim = c(NA, NA), ylim = c(NA, NA), sim.data.title = NULL)
{
    if (is.null(ADDT.plan.values$theta.vec.cr) & is.null(ADDT.plan.values$beta.cr) &
        is.null(ADDT.plan.values$sigma.cr))
        competing.risk <- F
    else competing.risk <- T
    save.Random.seed <- .Random.seed
    ADDT.test.plan <- hframe.to.vframe(ADDT.test.plan)
    distribution <- ADDT.plan.values$distribution
    transformation.time <- ADDT.plan.values$transformation.time
    transformation.x <- fix.inverse.relationship(ADDT.plan.values$transformation.x)
    number.parameters <- 3 + length(transformation.x)
    transformation.response <- ADDT.plan.values$transformation.response
    theta <- ADDT.plan.values$theta.vec
    theta.hat.star <- matrix(NA, nrow = number.sim, ncol = number.parameters)
    vcv <- matrix(NA, nrow = number.sim, ncol = ((number.parameters) *
        (number.parameters + 1))/2)
    likelihood <- rep(NA, length = number.sim)
    ierstuff <- rep(NA, length = number.sim)
    the.allocation <- allocation(ADDT.test.plan)
    sample.size <- sum(the.allocation)
    the.matrix <- matrix(NA, ncol = ncol(ADDT.test.plan), nrow = sample.size)
    allocation.column <- attr(ADDT.test.plan, "allocation.column")
    krow <- 0
    for (i in 1:nrow(ADDT.test.plan)) {
        allocation.now <- the.allocation[i, ]
        if (allocation.now > 0) {
            for (j in 1:allocation.now) {
                krow <- krow + 1
                the.matrix[krow, ] <- unlist(ADDT.test.plan[i,
                  , drop = F])
            }
        }
    }
    response.units <- get.response.units(ADDT.plan.values)
    attr(ADDT.test.plan, response.units) <- response.units
    ADDT.test.plan.names <- dimnames(ADDT.test.plan)[[2]]
    names(ADDT.test.plan.names) <- ADDT.test.plan.names
    ADDT.test.plan.names[allocation.column] <- response.units
    cat("ADDT.test.plan.names=", ADDT.test.plan.names, "\n")
    dimnames(the.matrix) <- list(as.character(1:nrow(the.matrix)),
        ADDT.test.plan.names)
    response.column <- my.strip.blanks(attr(ADDT.test.plan, response.units),
        FillChar = ".")
    if (is.null(sim.data.title))
        sim.data.title <- "Simulated Data"
    the.data.frame <- data.frame(the.matrix, Censor = rep("Exact",
        length = nrow(the.matrix)))
    data.ddd <- frame.to.ddd(the.data.frame, response.column = response.column,
        time.column = attr(ADDT.test.plan, "time.columns"), x.columns = attr(ADDT.test.plan,
            "levels.columns"), censor.column = "Censor", data.title = sim.data.title,
        response.units = get.response.units(ADDT.plan.values),
        time.units = get.time.units(ADDT.test.plan))
    time.tran <- f.relationship(times(data.ddd), transformation.time)
    x.tran <- xmat(data.ddd)
    for (i in 1:ncol(x.tran)) {
        x.tran[, i] <- f.relationship(x.tran[, i], subscript.relationship(transformation.x,
            i))
    }
    beta2.names <- paste("beta", seq(2, length(ADDT.plan.values$accelvar.units) +
        1), sep = "")
    beta.x <- as.matrix(x.tran) %*% as.matrix(theta[beta2.names],
        ncol = 1)
    fixed.part <- theta["beta0"] + theta["beta1"] * time.tran *
        exp(beta.x)
    if (map.SMRDDebugLevel() > 0)
        cat("Number of detail to show=", plotem, "\n")
    if (competing.risk) {
        theta.cr <- ADDT.plan.values$theta.vec.cr
        beta2.names.cr <- paste(beta2.names, ".cr", sep = "")
        beta.x.cr <- as.matrix(x.tran) %*% as.matrix(theta.cr[beta2.names.cr],
            ncol = 1)
        fixed.part.cr <- theta.cr["beta0.cr"] + theta.cr["beta1.cr"] *
            time.tran * exp(beta.x.cr)
    }
    number.good <- 0
    for (i in 1:number.sim) {
        the.response <- f.relationshipinv(fixed.part + quant(runif(sample.size),
            distribution) * theta["sigma"], transformation.response)
        if (competing.risk) {
            the.response.cr <- f.relationshipinv(fixed.part.cr +
                quant(runif(sample.size), distribution) * theta["sigma"],
                transformation.response)
            the.censor.column <- ifelse(the.response.cr > the.response,
                "Exact", "Right")
            data.ddd[, "Censor"] <- the.censor.column
            the.response <- pmin(the.response.cr, the.response)
        }
        data.ddd[, response.column] <- the.response
        cat("\nStart simulation", i, "of", number.sim, "\n")
        assign(envir = .frame0,  inherits = TRUE,"last.sim.ADDT.ddd", data.ddd)
        if (i <= as.numeric(plotem)) {
            dest.degrad.mle.out <- groupm.Dest.Degrad(data.ddd,
                distribution = distribution, transformation.response = transformation.response,
                transformation.x = ADDT.plan.values$transformation.x,
                transformation.time = transformation.time, xlim = xlim,
                ylim = ylim, FailLevel = ADDT.plan.values$FailLevel)
      } else dest.degrad.mle.out <- dest.degrad.mle(data.ddd,
            distribution = distribution, transformation.response = transformation.response,
            transformation.x = ADDT.plan.values$transformation.x,
            transformation.time = transformation.time)
        if (map.SMRDDebugLevel() >= 4) {
            cat("**************grad\n")
            print(dest.degrad.mle.out$grad)
            cat("**************origparam\n")
            print(dest.degrad.mle.out$origparam)
            cat("**************t.param\n")
            print(dest.degrad.mle.out$t.param)
        }
        if (is.null(dest.degrad.mle.out$grad) || any(is.na(dest.degrad.mle.out$grad)) ||
            any(abs(dest.degrad.mle.out$grad) > 0.1)) {
            if (!is.null(dest.degrad.mle.out$grad))
                warning(paste("Large gradient", paste(format(dest.degrad.mle.out$grad),
                  collapse = ",")))
            file.name <- paste("ProblemData", floor(runif(1) *
                1e+07), ".ddd", sep = "")
            assign(envir = .frame0,  inherits = TRUE,file.name, data.ddd)
            cat("\nCheck stored data in", file.name, "\n")
        }
        else {
            number.good <- number.good + 1
            theta.hat.star[number.good, ] <- dest.degrad.mle.out$origparam
            likelihood[number.good] <- dest.degrad.mle.out$max.log.like
            ierstuff[number.good] <- dest.degrad.mle.out$ierror
            if (ierstuff[number.good] > 0)
                InsulationBrkdwn.groupi.Dest.Degrad.out <- groupi.Dest.Degrad(data.ddd,
                  distribution = distribution, transformation.response = transformation.response,
                  transformation.time = transformation.time)
            vcvm <- dest.degrad.mle.out$origparamvcv
            vcv[number.good, ] <- vcvm[row(vcvm) >= col(vcvm)]
        }
    }
    if (number.good == 0)
        stop("No good simulations results returned")
    theta.hat.star <- theta.hat.star[1:number.good, , drop = F]
    length(likelihood) <- number.good
    length(ierstuff) <- number.good
    vcv <- vcv[1:number.good, , drop = F]
    param.names <- c("beta0", "beta1", paste("beta", 2:(length(transformation.x) +
        1), sep = ""), "sigma")
    dimnames(theta.hat.star) <- list(NULL, param.names)
    dimnames(vcv) <- list(NULL, get.vcv.names(param.names))
    return.list <- cbind(theta.hat.star, vcv)
    attr(return.list, "model") <- dest.degrad.mle.out$model
    attr(return.list, "save.Random.seed") <- save.Random.seed
    attr(return.list, "plan.values") <- ADDT.plan.values
    attr(return.list, "plan") <- ADDT.test.plan
    attr(return.list, "theta") <- theta
    attr(return.list, "ierstuff") <- ierstuff
    attr(return.list, "likelihood") <- likelihood
    attr(return.list, "title") <- "ADDT Simulation Results"
    oldClass(return.list) <- c("simulate.ADDT.out", "matrix")
    MysetOldClass(attr(return.list, "class"))
    cat("The last simulated ADDT data set has been saved in last.sim.ADDT.ddd\n")
    return(return.list)
}
