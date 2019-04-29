mlest.start.values <-
function (data.ld, distribution = "normal", intercept = T, max.iterations = 100,
   debug1= F)
{
    the.xmat <- as.matrix(xmat(data.ld))
    the.case.weights <- case.weights(data.ld)
    the.censor.codes <- censor.codes(data.ld)
    the.truncation.codes <- truncation.codes(data.ld)
    the.truncation.response <- truncation.response(data.ld)
    if (!is.null(the.truncation.response)) {
        nty <- ncol(the.truncation.response)
    }
    the.xmat.full <- the.xmat
    the.response.orig <-Response(data.ld)
    ny <- ncol(the.response.orig)
    if (is.logdist(distribution))
        the.response.full <- sqrt(Response(data.ld)[, 1, drop = F] *
           Response(data.ld)[, ny, drop = F])
    else the.response.full <- (Response(data.ld)[, 1, drop = F] +
       Response(data.ld)[, 1, drop = F])/2
    the.censor.full <- the.censor.codes
    base.the.weights <- the.case.weights
    marker <- rep("none", length = length(the.censor.codes))
    if (!is.null(the.truncation.response) && any(the.truncation.codes ==
        3)) {
        number.left.trunc <- length(the.truncation.codes[the.truncation.codes ==
            3])
        the.xmat.full <- rbind(the.xmat.full, the.xmat[the.truncation.codes ==
            3, , drop = F])
        the.response.full <- rbind(the.response.full, the.truncation.response[the.truncation.codes ==
            3, 1, drop = F])
        the.censor.full <- c(the.censor.full, rep(3, length = number.left.trunc))
        base.the.weights <- c(base.the.weights, the.case.weights[the.truncation.codes ==
            3])
        marker <- c(marker, rep("left", length = number.left.trunc))
    }
    if (!is.null(the.truncation.response) && any(the.truncation.codes ==
        2)) {
        number.right.trunc <- length(the.truncation.codes[the.truncation.codes ==
            2])
        the.xmat.full <- rbind(the.xmat.full, the.xmat[the.truncation.codes ==
            2, , drop = F])
        the.response.full <- rbind(the.response.full, the.truncation.response[the.truncation.codes ==
            2, nty, drop = F])
        the.censor.full <- c(the.censor.full, rep(2, length = number.right.trunc))
        base.the.weights <- c(base.the.weights, the.case.weights[the.truncation.codes ==
            2])
        marker <- c(marker, rep("right", length = number.right.trunc))
    }
    if (is.logdist(distribution)) {
        the.response.full <- logb(the.response.full)
  } else {
        the.response.full <- (the.response.full)
    }
    bad.values <- is.infinite(the.response.full) | is.nan(the.response.full) |
        is.na(the.response.full)
    if (any(bad.values)) {
        bad.values.indices <- which(bad.values)
        the.xmat.full.tmp <- the.xmat.full[-bad.values.indices,
            ]
        the.case.weights.tmp <- the.case.weights[-bad.values.indices]
        the.response.full <- the.response.full[-bad.values.indices]
  } else {
        the.xmat.full.tmp <- the.xmat.full
        the.case.weights.tmp <- the.case.weights
        the.response.full.tmp <- the.response.full
    }
    if (intercept) {
        if (all(abs(the.xmat.full.tmp[, 1] - 1) < 1e-10))
            the.xmat.full.tmp <- the.xmat.full.tmp[, -1, drop = F]
        the.fit <- lm(the.response.full.tmp ~ the.xmat.full.tmp)
  } else {
        the.fit <- lm(the.response.full.tmp ~ -1 + the.xmat.full.tmp)
    }
    mu <- predict(the.fit)
    sigma <- sqrt(var(residuals(the.fit)))
    theta <- c(coefficients(the.fit), sigma)
    names(theta) <- NULL
    if (debug1)
        print(c(iter = 0, theta = theta))
    thetahold <- theta
    for (i in 1:max.iterations) {
        if (i > 1) {
            if (all(abs(thetahold - theta) < 0.01))
                break
            thetahold <- theta
        }
        pseudo.response <- the.response.full
        if (any(the.censor.full == 3)) {
            z3 <- (the.response.full[the.censor.full == 3] -
                mu[the.censor.full == 3])/sigma
            pseudo.response[the.censor.full == 3] <- mu[the.censor.full ==
                3] - (sigma * wqmf.phis(z3, distribution))/wqmf.phibf(z3,
                distribution)
        }
        if (any(the.censor.full == 2)) {
            z2 <- (the.response.full[the.censor.full == 2] -
                mu[the.censor.full == 2])/sigma
            pseudo.response[the.censor.full == 2] <- mu[the.censor.full ==
                2] + (sigma * wqmf.phis(z2, distribution))/(1 -
                wqmf.phibf(z2, distribution))
        }
        the.case.weights <- base.the.weights
        if (any(marker == "left")) {
            z3 <- (the.response.full[marker == "left"] - mu[marker ==
                "left"])/sigma
            the.case.weights[marker == "left"] <- the.case.weights[marker ==
                "left"] * wqmf.phibf(z3, distribution)
        }
        if (any(marker == "right")) {
            z2 <- (the.response.full[marker == "right"] - mu[marker ==
                "right"])/sigma
            the.case.weights[marker == "right"] <- the.case.weights[marker ==
                "right"] * (1 - wqmf.phibf(z2, distribution))
        }
        bad.values <- is.infinite(pseudo.response) | is.na(pseudo.response) |
            is.na(pseudo.response)
        if (any(bad.values)) {
            bad.values.indices <- which(bad.values)
            the.xmat.full.tmp <- the.xmat.full[-bad.values.indices,
                ]
            the.case.weights.tmp <- the.case.weights[-bad.values.indices]
            pseudo.response.tmp <- pseudo.response[-bad.values.indices]
      } else {
            the.xmat.full.tmp <- the.xmat.full
            the.case.weights.tmp <- the.case.weights
            pseudo.response.tmp <- pseudo.response
        }
        if (intercept) {
            if (all(abs(the.xmat.full.tmp[, 1] - 1) < 1e-10))
                the.xmat.full.tmp <- the.xmat.full.tmp[, -1,
                  drop = F]
            the.fit <- lm(pseudo.response.tmp ~ the.xmat.full.tmp,
                weights = the.case.weights.tmp)
      } else {
            the.fit <- lm(pseudo.response.tmp ~ -1 + the.xmat.full.tmp,
                weights = the.case.weights.tmp)
        }
        mu <- predict(the.fit)
        sigma <- sqrt(var(residuals(the.fit)))
        theta <- c(coefficients(the.fit), sigma)
        names(theta) <- NULL
        if (debug1)
            print(c(iter = i, theta = theta))
    }
    theta[length(theta)] <- theta[length(theta)]/2
    return(theta)
}
