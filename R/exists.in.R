exists.in <-
function (the.element, the.list, frame)
{
    if (missing(frame)) {
        if (exists(the.list)) {
            if (!is.null(get(envir = .frame0, the.list)[[the.element]]))
                return(TRUE)
            else return(FALSE)
      } else return(FALSE)
  } else {
        if (exists(the.list)) {
            if (!is.null(get(envir = .frame0, the.list)[[the.element]]))
                return(TRUE)
            else return(FALSE)
      } else return(FALSE)
    }
}
