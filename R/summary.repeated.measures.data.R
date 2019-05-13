#' @export
summary.repeated.measures.data <-
function (object,...)
{
    Unit.marker <- attr(object, "Unit.marker")
    response.column <- attr(object, "response.column")
    time.column <- attr(object, "time.column")
    time.units <- attr(object, "time.units")
    data.note <- attr(object, "data.note")
    response.units <- attr(object, "response.units")
    xlabel <- attr(object, "xlabel")
    response.units <- get.response.units(object)
    the.x.columns <- get.x.columns(object)
    number.cases <- length(Unit.marker)
    response <-Response(object)
    cat(paste("\n\nData set name: ", get.data.title(object),
        "\n", sep = ""))
    if (length(data.note) > 0 && data.note != "")
        cat(paste("\n", data.note, "\n", sep = ""))
    cat(paste("\nNumber observational units =", length(unique(Unit.marker)),
        "\n"))
    cat(paste("Number of rows in data matrix=", number.cases,
        "\n"))
    cat(paste("Response units: ", response.units, "\n"))
    cat(paste("Time units: ", time.units, "\n"))
    cat(paste("Response minimum: ", min(response), "\n"))
    cat(paste("Response maximum: ", max(response), "\n"))
    if (is.null(the.x.columns)) {
        cat("No explanatory variables\n")
    } else {
        xmat <- object[the.x.columns]
        x.strings <- apply(xmat, 1, paste, collapse = " ")
        uniquex <- unique(x.strings)
        if (is.list(xmat)) {
            numeric.columns.list <- lapply(xmat, is.numeric)
            numeric.columns <- unlist(numeric.columns.list)
      } else {
            numeric.columns <- apply(xmat, 2, is.numeric)
        }
        if (any(numeric.columns)) {
            the.mean <- apply(xmat[, numeric.columns, drop = F],
                2, mean)
            the.sd <- sqrt(apply(xmat[, numeric.columns, drop = F],
                2, var))
            the.cv <- the.sd/the.mean
            xsummary <- cbind(apply(xmat[, numeric.columns, drop = F],
                2, min), apply(xmat[, numeric.columns, drop = F],
                2, max), the.mean, the.sd, the.cv)
            dimnames(xsummary) <- list(xlabel[numeric.columns],
                c("min", "max", "mean", "sd", "cv"))
            cat(paste("\nSummary of numeric columns in X matrix: ",
                "\n"))
            print(xsummary)
        }
        ncolx <- ncol(xmat)
        c1 <- rep(1, length(uniquex))
        table <- data.frame(xmat[1:length(uniquex), ], c1, c1,
            c1)
        for (i in 1:length(uniquex)) {
            the.stuff <- uniquex[i] == x.strings
            table[i, ncolx + 1] <- min(response[the.stuff, ])
            table[i, ncolx + 2] <- max(response[the.stuff, ])
            table[i, ncolx + 3] <- length(unique(Unit.marker[the.stuff]))
            the.ones <- (1:nrow(response))[the.stuff]
            for (j in 1:ncolx) {
                if (is.factor(xmat[the.ones[1], j]))
                  table[i, j] <- as.character(xmat[the.ones[1],
                    j])
                else table[i, j] <- xmat[the.ones[1], j]
            }
        }
        table.names <- list(1:nrow(table), c(xlabel, "min-Resp",
            "max-Resp", "Number"))
        dimnames(table) <- table.names
        check.zero <- function(x) {
            any(x != 0)
        }
        any.non.zero <- apply(table, 2, check.zero)
        if (nrow(table) < 16) {
            cat(paste("\nList of unique X values (or combinations)",
                "\n"))
            print(table[, any.non.zero])
        }
    }
    cat(paste("\n\n", sep = ""))
    invisible()
}
