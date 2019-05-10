wqm.pretty <-
function (x, 
          min.n = nint%/%3, 
          shrink.sml = 0.75, 
          high.u.bias = 1.5,
          u5.bias = 0.5 + 1.5 * high.u.bias, 
          eps.correct = 0, 
          nint = NULL)
{
    if (is.null(nint)) nint <- 5
    
        pretty(x = x, 
               n = nint, 
               min.n = min.n, 
               shrink.sml = shrink.sml,
               high.u.bias = 1.5, 
               u5.bias = high.u.bias, 
               eps.correct = eps.correct)
        
}
