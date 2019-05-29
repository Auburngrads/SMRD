multiple.get.rmd.subset <-
function (data.rmd, markers, columns = 1:length(attr(data.rmd,
    "x.columns")))
{
    if (is.null(attr(data.rmd, "x.columns")))
        stop(paste("No xmat in:", get.data.title(data.rmd)))
    right.stuff <- get.theright.stuff(data.rmd, columns, markers)
    invisible(get.rmd.subset(data.rmd, right.stuff))
}
