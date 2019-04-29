summary.Dest.Degrad.data <-
function (object,...)
{
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
      first.response <- Response(data.ddd)[the.first.zeros]
      reps.found <- rep(F, length = length(the.unique.markers) -
                          1)
      for (i in 2:length(the.unique.markers)) {
        the.current.zeros <- the.markers == the.unique.markers[i] &
          zero.times
        current.response <- Response(data.ddd)[the.current.zeros]
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

    data.ddd <- ADDTZeroRepsFilter(object)
    data.ddd <- SMRD.sanity.Dest.Degrad.data(data.ddd)
    cat(paste("\nSummary of: ", get.data.title(data.ddd), "\n",
        sep = ""))
    number.cases <- nrow(Response(data.ddd))
    the.censor.codes <- censor.codes(data.ddd)
    the.case.weights <- case.weights(data.ddd)
    theResponse <- Response(data.ddd)
    if (all(the.censor.codes == 1)) {
        no.censoring <- T
  } else {
        no.censoring <- F
    }
    not.dummy <- the.case.weights > 0 & the.censor.codes > 0
    if (!is.null(get.data.note(data.ddd))) {
        the.characters <- string2char(get.data.note(data.ddd))
        print.note <- length(the.characters) > 1 && !all(the.characters ==
            " ")
        if (print.note)
            cat(paste("\n", parse.note(get.data.note(data.ddd)),
                "\n\n", sep = ""))
    }
    cat(paste("Number of rows in data matrix=", nrow(theResponse),
        "\n"))
    cat(paste("Time units: ", get.time.units(data.ddd), "\n"))
    cat(paste("Response units: ", get.response.units(data.ddd),
        "\n"))
    cat(paste("Response minimum: ", format(min(theResponse)),
        "\n"))
    cat(paste("Response maximum: ", format(max(theResponse)),
        "\n"))
    cat(paste("Number of cases in data set=", sum(the.case.weights[not.dummy]),
        "\n"))
    number.exact.fail <- sum(the.case.weights[not.dummy & the.censor.codes ==
        1])
    if (number.exact.fail > 0) {
        cat(paste("Number of exact observations in data set=",
            number.exact.fail, "\n"))
    }
    number.right.censored <- sum(the.case.weights[not.dummy &
        the.censor.codes == 2])
    if (number.right.censored > 0) {
        cat(paste("Number of right censored observations in data set=",
            number.right.censored, "\n"))
    }
    number.left.censored <- sum(the.case.weights[not.dummy &
        the.censor.codes == 3])
    if (number.left.censored > 0) {
        cat(paste("Number of left censored observations in data set=",
            number.left.censored, "\n"))
    }
    number.interval.censored <- sum(the.case.weights[not.dummy &
        the.censor.codes == 4])
    if (number.interval.censored > 0) {
        cat(paste("Number of interval censored observations in data set=",
            number.interval.censored, "\n"))
    }
    number.sinterval.censored <- sum(the.case.weights[not.dummy &
        the.censor.codes == 5])
    if (number.sinterval.censored > 0) {
        cat(paste("Number of small-interval observations in data set=",
            number.sinterval.censored, "\n"))
    }
    if (no.censoring)
        cat("No censoring information\n")
    the.xmat <- data.frame(times(data.ddd), xmat(data.ddd))
    if (is.null(the.xmat) || length(the.xmat) == 0) {
        cat("No explanatory variables\n")
  } else {
        x.strings <- apply(the.xmat, 1, paste, collapse = " ")
        uniquex <- unique(x.strings)
        xlabel <- dimnames(the.xmat)[[2]]
        if (is.list(the.xmat)) {
            numeric.columns.list <- lapply(the.xmat, is.numeric)
            numeric.columns <- unlist(numeric.columns.list)
      } else {
            numeric.columns <- apply(the.xmat, 2, is.numeric)
        }
        if (any(numeric.columns)) {
            the.mean <- apply(the.xmat[, numeric.columns, drop = F],
                2, mean)
            the.sd <- sqrt(apply(the.xmat[, numeric.columns,
                drop = F], 2, var))
            the.cv <- the.sd/the.mean
            xsummary <- cbind(apply(the.xmat[, numeric.columns,
                drop = F], 2, min), apply(the.xmat[, numeric.columns,
                drop = F], 2, max), the.mean, the.sd, the.cv)
            dimnames(xsummary) <- list(xlabel[numeric.columns],
                c("min", "max", "mean", "sd", "cv"))
            cat(paste("\nSummary of numeric columns in X matrix: ",
                "\n"))
            print(xsummary)
        }
        ncolx <- ncol(the.xmat)
        c1 <- rep(1, length(uniquex))
        the.table <- data.frame(the.xmat[1:length(uniquex), ],
            c1, c1, c1, c1, c1, c1)
        for (i in 1:length(uniquex)) {
            the.stuff <- uniquex[i] == x.strings
            the.table[i, ncolx + 1] <- min(theResponse[the.stuff])
            the.table[i, ncolx + 2] <- max(theResponse[the.stuff])
            the.table[i, ncolx + 3] <- sum(the.case.weights[the.stuff &
                the.censor.codes == 1])
            the.table[i, ncolx + 4] <- sum(the.case.weights[the.stuff &
                the.censor.codes == 2])
            the.table[i, ncolx + 5] <- sum(the.case.weights[the.stuff &
                the.censor.codes == 3])
            the.table[i, ncolx + 6] <- sum(the.case.weights[the.stuff &
                the.censor.codes == 4])
            the.ones <- (1:nrow(theResponse))[the.stuff]
            for (j in 1:ncolx) {
                if (is.factor(the.xmat[the.ones[1], j]))
                  the.table[i, j] <- as.character(the.xmat[the.ones[1],
                    j])
                else the.table[i, j] <- the.xmat[the.ones[1],
                  j]
            }
        }
        the.table.names <- list(1:nrow(the.table), c(xlabel,
            "min-Resp", "max-Resp", "#exact", "#rcen", "#lcen",
            "#intcen"))
        dimnames(the.table) <- the.table.names
        check.zero <- function(x) {
            any(x != 0)
        }
        any.non.zero <- apply(the.table, 2, check.zero)
        if (nrow(the.table) < 100) {
            cat(paste("\nList of unique X values (or combinations)",
                "\n"))
            print(the.table[, any.non.zero])
      } else {
            cat("\nnumber of unique x conditions =", nrow(the.table),
                "\n\n")
        }
    }
    cat("\n")
    invisible()
}
