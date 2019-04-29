strip.bad.subsets <-
function (data.ld, stresses = get.x.markers(data.ld, group.var = group.var),
    group.var = 1:ncol(xmat(data.ld)))
{
    the.right.stuff <- rep(T, nrow(Response(data.ld)))
    for (i in 1:length(stresses)) {
        data.subset.d <- multiple.get.data.subset(data.ld, stresses[i],
            columns = group.var)
        if (!good.data(data.subset.d, number.needed = 1)) {
            the.right.stuff <- the.right.stuff & !.right.stuff(data.subset.d)
        }
    }
    return(get.data.subset(data.ld, the.right.stuff, "Estimable Subsets"))
}
