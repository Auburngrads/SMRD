wqm.objects.summary <-
function (where = 1, mode = "all", all.names = FALSE)
{
    if (is.R()) {
        object.list <- objects(pos = where, all.names = all.names)
    }
    else {
        object.list <- objects(pos = where)
    }
    class.get <- function(str) class(get(envir = .frame0, str))[1]
    the.class <- unlist(lapply(object.list, class.get))
    the.frame <- data.frame(the.class)
    if (!is.R()) {
        dimnames(the.frame) <- list(row.names = object.list,
            names = "data.class")
    }
    else {
        names(the.frame) <- "data.class"
        rownames(the.frame) <- object.list
    }
    if (mode == "all")
        the.frame
    else {
        the.frame[the.frame[, "data.class"] == mode, , drop = F]
    }
}
