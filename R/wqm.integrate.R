wqm.integrate <-
function (f, lower, upper, subdivisions = 100, rel.tol = .Machine$double.eps^0.25, 
    abs.tol = rel.tol, keep.xy = FALSE, aux = NULL, ...) 
{
    initial.results <- integrate(f = f, lower = lower, upper = upper, 
        subdivisions = subdivisions, rel.tol = rel.tol, abs.tol = abs.tol, 
        keep.xy = keep.xy, aux = aux, ...)
    if (!is.R()) {
        results <- initial.results
    }
    else {
        results <- initial.results
        results$integral <- results$value
        results$value <- NULL
    }
    results
}
