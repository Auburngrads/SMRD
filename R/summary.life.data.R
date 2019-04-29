summary.life.data <-
function (object, printem = T, print.limit = 50,...)
{
    lda.type <- data.object.type(data.ld = object)
    if (lda.type != "frame.centered")
        cat("\nOld-style life data object; consider rebuilding it\n")
    data.ld <- SMRD.sanity.life.data(x = object)
    the.response <- Response(data.ld)
    number.cases <- nrow(the.response)
    the.case.weights <- case.weights(data.ld)
    if (is.null(the.case.weights))
        the.case.weights <- rep(1, number.cases)
    the.censor.codes <- censor.codes(data.ld)
    if (is.null(the.censor.codes)) {
        the.censor.codes <- rep(1, number.cases)
        no.censoring <- T
  } else {
        no.censoring <- F
    }
    not.dummy <- the.case.weights > 0 & the.censor.codes > 0
    if (printem) {
        cat(paste("\nSummary of: ", get.data.title(data.ld),
            "\n", sep = ""))
        data.note <- get.data.note(data.ld)
        if (!is.null(data.note)) {
            the.characters <- string2char(data.note)
            print.note <- length(the.characters) > 1 && !all(the.characters ==
                " ")
            if (print.note)
                cat(paste("\n", parse.note(data.note), "\n\n",
                  sep = ""))
        }
        cat(paste("Number of rows in data matrix=", nrow(the.response),
            "\n"))
        cat(paste("Response units: ", get.time.units(data.ld),
            "\n"))
        cat(paste("Response minimum: ", format(min(the.response)),
            "\n"))
        cat(paste("Response maximum: ", format(max(the.response)),
            "\n"))
        cat(paste("Number of cases in data set=", sum(the.case.weights[not.dummy]),
            "\n"))
        number.exact.fail <- sum(the.case.weights[not.dummy &
            the.censor.codes == 1])
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
        the.failure.modes <- failure.modes(data.ld)
        if (!is.null(the.failure.modes)) {
            cat("The unique reported failure modes are\n", paste(unique(as.character(the.failure.modes)),
                collapse = ", "), "\n")
        }
        truncation.codes <- truncation.codes(data.ld)
        ty <- truncation.response(data.ld)
        if (!is.null(truncation.codes) && !is.null(ty)) {
            if (is.null(truncation.codes) || is.null(ty))
                stop("If either truncation.codes or ty is specified, both must be specified")
            if (length(truncation.codes) != number.cases)
                stop(paste("Number of truncation codes ",
                  length(truncation.codes), " is wrong"))
            ty <- as.matrix(ty)
            if (nrow(ty) != number.cases)
                stop(paste("Number of truncation times ",
                  length(ty), " is wrong"))
            nty <- ncol(ty)
            cat(paste("Number of right truncated observations in data set=",
                sum(the.case.weights[not.dummy & truncation.codes ==
                  2]), "\n"))
            cat(paste("Number of left truncated observations in data set=",
                sum(the.case.weights[not.dummy & truncation.codes ==
                  3]), "\n"))
            cat(paste("Number of interval truncated observations in data set=",
                sum(the.case.weights[not.dummy & truncation.codes ==
                  4]), "\n"))
     } else {
            cat(paste("No truncation information\n"))
            nty <- 0
            truncation.codes <- rep(1, length(the.censor.codes))
            ty <- rep(0, length(the.censor.codes))
        }
        the.xmat <- xmat(data.ld)
        if (is.null(the.xmat)) {
            cat("No explanatory variables\n")
      } else {
            if (nrow(the.xmat) < 200) {
                x.strings <- apply(the.xmat, 1, paste, collapse = " ")
                uniquex <- unique(x.strings)
                if (is.list(the.xmat)) {
                  numeric.columns.list <- lapply(the.xmat, is.numeric)
                  numeric.columns <- unlist(numeric.columns.list)
              } else {
                  numeric.columns <- apply(the.xmat, 2, is.numeric)
                }
                if (any(numeric.columns)) {
                  the.mean <- apply(the.xmat[, numeric.columns,
                    drop = F], 2, mean)
                  the.sd <- sqrt(apply(the.xmat[, numeric.columns,
                    drop = F], 2, var))
                  the.cv <- the.sd/the.mean
                  xsummary <- cbind(apply(the.xmat[, numeric.columns,
                    drop = F], 2, min), apply(the.xmat[, numeric.columns,
                    drop = F], 2, max), the.mean, the.sd, the.cv)
                  rownames(xsummary) <- names(get.xlabel(data.ld)[numeric.columns])
                  colnames(xsummary) <- c("min", "max", "mean", "sd", "cv")
                  cat(paste("\nSummary of numeric columns in X matrix: ",
                    "\n"))
                  print(xsummary)
                }
                ncolx <- ncol(the.xmat)
                c1 <- rep(1, length(uniquex))
                the.table <- data.frame(the.xmat[1:length(uniquex),
                  ], c1, c1, c1, c1, c1, c1, c1, c1)
                for (i in 1:length(uniquex)) {
                  the.stuff <- uniquex[i] == x.strings
                  the.table[i, ncolx + 1] <- min(the.response[the.stuff])
                  the.table[i, ncolx + 2] <- max(the.response[the.stuff])
                  the.table[i, ncolx + 3] <- mean(the.response[the.stuff])
                  the.sd <- sqrt(var(the.response[the.stuff]))
                  if (is.na(the.sd))
                    the.sd <- 0
                  the.table[i, ncolx + 4] <- the.sd
                  the.table[i, ncolx + 5] <- sum(the.case.weights[the.stuff &
                    the.censor.codes == 1])
                  the.table[i, ncolx + 6] <- sum(the.case.weights[the.stuff &
                    the.censor.codes == 2])
                  the.table[i, ncolx + 7] <- sum(the.case.weights[the.stuff &
                    the.censor.codes == 3])
                  the.table[i, ncolx + 8] <- sum(the.case.weights[the.stuff &
                    the.censor.codes == 4])
                  the.table[i, ncolx + 9] <- sum(the.case.weights[the.stuff])
                  the.ones <- (1:nrow(the.response))[the.stuff]
                  for (j in 1:ncolx) {
                    if (is.factor(the.xmat[the.ones[1], j])) {
                      the.table[i, j] <- as.character(the.xmat[the.ones[1],j])
                      
                  } else { the.table[i, j] <- the.xmat[the.ones[1],j]  }
                  
                    }
                    
                }
                rownames(the.table) <- 1:nrow(the.table)
                colnames(the.table) <- c(names(get.xlabel(data.ld)),"min-Resp", "max-Resp", 
                                         "mean-Resp","SD-Resp","#Exact", "#R-cen", "#L-cen", 
                                         "#Int-cen", "Total")
                check.zero <- function(x) {
                  any(x != 0)
                }
                some.censoring <- any(the.table[, ncolx + 9] !=
                  the.table[, ncolx + 5])
                if (some.censoring) {
                  the.table <- the.table[, -(ncolx + c(3, 4))]
                }
                any.non.zero <- apply(the.table, 2, check.zero)
                if (nrow(the.table) < print.limit) {
                  cat(paste("\nList of unique X values (or combinations)",
                    "\n"))
                  print(the.table[, any.non.zero])
              } else {
                  cat("\nnumber of unique x conditions =", nrow(the.table),
                    "\n\n")
                }
            }
        }
        cat("\n")
    }
    results <- list(number.cases = sum(the.case.weights[not.dummy]))
    invisible(results)
}
