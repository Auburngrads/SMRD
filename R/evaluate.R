#
#
#

evaluate <-
function (x,...)
UseMethod("evaluate")

#
#

evaluate.ADDT.test.plan <-
  function (x, ADDT.plan.values, use.condition, FailLevel,
            quantile.of.interest = 0.1, conf.level = GetSMRDDefault("SMRD.ConfLevel")/100,
            printem = T,...)
  {
    if (get.time.units(ADDT.plan.values) != get.time.units(x))
      warning(paste("\nPlan value time units =", get.time.units(ADDT.plan.values),
                    "is not the same as \nTest plan time units =", get.time.units(x),
                    "\n"))
    if (missing(use.condition)) {
      use.condition <- ADDT.plan.values$use.condition
      if (is.null(use.condition))
        stop("Use conditions must be specified")
    }
    use.condition <- string.to.frame(use.condition)
    if (missing(FailLevel)) {
      FailLevel <- ADDT.plan.values$FailLevel
      if (is.null(FailLevel))
        stop("FailLevel must be specified")
    }
    frame.type <- attr(x, "frame.type")
    switch(frame.type, vframe = {
    }, hframe = {
      x <- hframe.to.vframe(x)
    }, {
      stop(paste("Bad frame.type,", frame.type))
    })
    ADDT.vcv.out <- ADDT.vcv(ADDT.plan.values = ADDT.plan.values, ADDT.test.plan = x)
    vcv.gamma <- ADDT.vcv.out$the.tran.vcv
    distribution <- ADDT.plan.values$distribution
    transformation.time <- ADDT.plan.values$transformation.time
    transformation.x <- ADDT.plan.values$transformation.x
    for (i in 1:length(transformation.x)) {
      if (generic.relationship.name(transformation.x[i]) ==
          "Arrhenius")
        transformation.x[i] <- "Arrhenius3"
    }
    transformation.response <- ADDT.plan.values$transformation.response
    theta.hat <- ADDT.plan.values$theta.vec
    ADDT.model <- .pseudo.model(ADDT.plan.values, x)
    gamma.hat <- .f.ADDT.stableparam(theta.hat, model = ADDT.model)
    theta2.hat <- .f.ADDT.origparam(gamma.hat, model = ADDT.model)
    if (map.SMRDDebugLevel() >= 4) {
      cat("\nin evaluate.x t,g,t2=\n", format(theta.hat),
          "\n", format(theta2.hat), "\n", format(gamma.hat),
          "\n")
      results.orig <- fx.ADDT.life.quantile(theta.hat = theta.hat,
                                            p = quantile.of.interest[1], distribution = distribution,
                                            FailLevel = FailLevel, xuse = use.condition, transformation.response = transformation.response,
                                            transformation.x = transformation.x, transformation.time = transformation.time,
                                            trans.of.quantile = F)
      results.trans <- fx.ADDT.life.quantile.gamma(gamma.hat = gamma.hat,
                                                   p = quantile.of.interest[1], distribution = distribution,
                                                   FailLevel = FailLevel, xuse = use.condition, transformation.response = transformation.response,
                                                   transformation.x = transformation.x, transformation.time = transformation.time,
                                                   model = ADDT.model, trans.of.quantile = F)
      print(results.orig)
      print(results.trans)
      cat("tran vcv\n")
      print(ADDT.vcv.out$the.tran.vcv)
      cat("orig vcv\n")
      print(ADDT.vcv.out$the.orig.vcv)
      cat("the.tran.fim\n")
      print(ADDT.vcv.out$the.tran.fim)
    }
    zfact <- qnorm(1 - (1 - conf.level)/2)
    asd <- rep(NA, length(quantile.of.interest))
    Rfact <- rep(NA, length(quantile.of.interest))
    the.quantile <- rep(NA, length(quantile.of.interest))
    for (ivec in 1:length(quantile.of.interest)) {
      if (is.logdist(distribution))
        is.log.message <- "log"
      else is.log.message <- ""
      answer <- f.gendeltamethod(vcv.gamma, gamma.hat, fx.ADDT.life.quantile.gamma,
                                 p = quantile.of.interest[ivec], distribution = distribution,
                                 FailLevel = FailLevel, xuse = use.condition, transformation.response = transformation.response,
                                 transformation.x = transformation.x, transformation.time = transformation.time,
                                 model = ADDT.model)
      asd[ivec] <- answer$se
      the.quantile[ivec] <- answer$vec
      if (map.SMRDDebugLevel() >= 4) {
        vcv.theta <- ADDT.vcv.out$the.orig.vcv
        answer <- f.gendeltamethod(vcv.theta, theta.hat,
                                   fx.ADDT.life.quantile, p = quantile.of.interest[ivec],
                                   distribution = distribution, FailLevel = FailLevel,
                                   xuse = use.condition, transformation.response = transformation.response,
                                   transformation.x = transformation.x, transformation.time = transformation.time)
        asd.check <- answer$se
        the.quantile.check <- answer$vec
        cat("Check in evaluate.x\n")
        print(c(asd[ivec], asd.check))
        print(c(the.quantile[ivec], the.quantile.check))
      }
      Rfact[ivec] <- exp((zfact * asd[ivec])/the.quantile[ivec])
      if (printem) {
        cat(paste("\nWith a failure definition of ", FailLevel,
                  " ", ADDT.plan.values$response.units, ",", sep = ""),
            "\nthe large sample approximate standard deviation\nof the",
            quantile.of.interest[ivec], is.log.message, "quantile at",
            paste(use.condition, ADDT.plan.values$accelvar.units,
                  collapse = ","), " =", format(asd[ivec], digits = 4),
            get.time.units(ADDT.plan.values), "and the",
            "\ncorresponding", percent.conf.level(conf.level),
            "confidence precision factor is R=", format(Rfact[ivec],
                                                        digits = 4), "\n\n")
      }
    }
    return.table <- cbind(Quantile = quantile.of.interest, value = the.quantile,
                          Ase = asd, `R-Factor` = Rfact)
    colnames <- dimnames(return.table)[[2]]
    colnames[2] <- get.time.units(ADDT.plan.values)
    dimnames(return.table) <- list(rep("", nrow(return.table)),
                                   colnames)
    print(return.table)
    attr(return.table, "the.tran.vcv") <- ADDT.vcv.out$the.tran.vcv
    invisible(return.table)
  }

#
#

evaluate.alt.test.plan <-
  function (x, ALT.plan.values, use.conditions, quantile.of.interest,
            conf.level = GetSMRDDefault("SMRD.ConfLevel")/100,...)
  {
    if (missing(use.conditions)) {
      if (is.null(ALT.plan.values$use.conditions))
        stop("\n Use conditions have not been specified.\n")
      use.conditions <- ALT.plan.values$use.conditions
    }
    relationship <- ALT.plan.values$relationship
    tran.use.conditions <- use.conditions
    for (j in 1:length(use.conditions)) {
      tran.use.conditions[j] <- f.relationship(use.conditions[j],
                                               subscript.relationship(relationship, j))
    }
    zfact <- qnorm(1 - (1 - conf.level)/2)
    distribution <- ALT.plan.values$distribution
    sample.size <- sum(allocation(x))
    vcv.out <- ALT.vcv(x, ALT.plan.values)
    vcv <- vcv.out$the.vcv
    if (map.SMRDDebugLevel() >= 5) {
      cat("in evaluate.x\n")
      print(vcv)
      print(tran.use.conditions)
    }
    asd <- rep(NA, length = length(quantile.of.interest))
    the.quantile <- rep(NA, length = length(quantile.of.interest))
    Rfact <- rep(NA, length = length(quantile.of.interest))
    for (ivec in 1:length(quantile.of.interest)) {
      eval.vec <- c(1, tran.use.conditions, quant(quantile.of.interest[ivec],
                                                  distribution))
      the.quantile[ivec] <- t(eval.vec) %*% ALT.plan.values$theta.vec
      asd[ivec] <- sqrt(t(eval.vec) %*% vcv %*% eval.vec)
      if (is.logdist(distribution)) {
        the.quantile[ivec] <- exp(the.quantile[ivec])
        asd[ivec] <- asd[ivec] * the.quantile[ivec]
        Rfact[ivec] <- exp(((zfact * asd[ivec])/the.quantile[ivec]))
        precision.name <- "R-Factor"
      }
      else {
        Rfact[ivec] <- zfact * asd[ivec]
        precision.name <- "Half-width"
      }
    }
    return.table <- cbind(Quantile = quantile.of.interest, value = the.quantile,
                          Ase = asd, `R-Factor` = Rfact)
    colnames <- dimnames(return.table)[[2]]
    colnames[2] <- get.time.units(ALT.plan.values)
    colnames[4] <- precision.name
    dimnames(return.table) <- list(rep("", nrow(return.table)),
                                   colnames)
    cat("\n\nTest plan summary:\n")
    print(vcv.out$plan.table)
    cat("\nEvaluation at use conditions", paste(use.conditions,
                                                attr(x, "accelvar.names"), collapse = ", "),
        "\n")
    return.table <- data.frame(return.table)
    attr(return.table, "sample.size") <- sample.size
    return(return.table)
  }
