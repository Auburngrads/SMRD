multiple.get.data.subset <-
function (data.ld, markers, columns = 1:ncol(xmat(data.ld)))
{
    right.stuff <- get.theright.stuff(data.ld, columns, markers)
    invisible(get.data.subset(data.ld, right.stuff))
}
