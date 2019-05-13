#' @export
summary.RMD.simulation.results <-
function (object,...)
{
    cat("\n\nSummary for simulation results object", deparse(substitute(object)),
        "\n")
    cat("Number of simulations=", nrow(object$results.matrix),
        "\n")
}
