plot.rmd.residual <-
function (x, title.option = GetSMRDDefault("SMRD.TitleOption"), xlab = NULL, ylab = "Residuals",
    outer.plot = F, order.groups = F, group.var = 1:length(get.x.columns(x)),...)
{
    data.rmd <- attr(x, "residual.rmd")
    if (is.null(data.rmd))
        stop("Residuals not found---perhpas all units were real failures.")
    results <- trellis.plot.repeated.measures.data(data.rmd,
        title.option = title.option, outer.plot = outer.plot,
        order.groups = order.groups, xlab = xlab)
    invisible(results)
}
