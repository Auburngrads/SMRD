failure.modes <-
function (data.d)
{
    failure.mode.column <- attr(data.d, "failure.mode.column")
    if (is.character(data.d) && length(data.d == 1))
        data.d <- get(envir = .frame0, data.d)
    if (is.null(failure.mode.column)) {
        return(data.d$failure.modes)
  } else {
        return(data.d[, failure.mode.column])
    }
}
