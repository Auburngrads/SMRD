#' @export
print.Dest.Degrad.data <-
function (x, includex = T, quote = F, prefix = "",...)
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

    if (!is.data.frame(x)) {
        print.default(x)
        cat("get.data.title(x), not a data.frame\n")
        return(NULL)
    }
    data.ddd <- ADDTZeroRepsFilter(x)
    cat(paste("Data from ", get.data.title(data.ddd), "\n"))
    obs.type <- c("Dummy", "Failure", "R-Censored", "L-Censored",
        "Interval", "Small-Interval")
    Case.weights <- case.weights(data.ddd)
    if (is.null(Case.weights) || all(Case.weights == 1)) {
        name.case.weights <- NULL
        Case.weights <- NULL
  } else {
        name.case.weights <- "Case.weights"
    }
    if (is.null(failure.modes(data.ddd))) {
        name.fail.modes <- NULL
        the.failure.modes <- NULL
  } else {
        name.fail.modes <- "Failure.Modes"
        the.failure.modes <- as.character(failure.modes(data.ddd))
    }
    Censor.codes <- censor.codes(data.ddd)
    if (is.null(Censor.codes) || length(unique(Censor.codes)) <=
        1) {
        name.censor.code <- NULL
        status <- NULL
  } else {
        name.censor.code <- "Status"
        status.ind <- match(Censor.codes, 0:5)
        status <- obs.type[status.ind]
    }
    the.frame <- my.data.frame(cbind(times(data.ddd),Response(data.ddd),
        status, Case.weights, the.failure.modes))
    if (!is.null(xmat(data.ddd)))
        the.frame <- data.frame(the.frame, xmat(data.ddd))
    names(the.frame) <- c("Times", dimnames(Response(data.ddd))[[2]],
        name.censor.code, name.case.weights, name.fail.modes,
        dimnames(xmat(data.ddd))[[2]])
    the.frame$Status <- status
    print.data.frame(the.frame)
    invisible(the.frame)
}
