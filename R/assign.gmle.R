assign.gmle <-
function (gmle.out, debug1 = F, monitor = F)
{
    assign(envir = .frame0, inherits = !TRUE,"log.like", gmle.out$log.like)
    assign(envir = .frame0, inherits = !TRUE,"monitor", monitor)
    assign(envir = .frame0, inherits = !TRUE,"model", gmle.out$model)
    assign(envir = .frame0, inherits = !TRUE,"debug1", debug1)
    assign(envir = .frame0, inherits = !TRUE,"data.ld", gmle.out$data.ld)
    assign(envir = .frame0, inherits = !TRUE,"special.stuff", gmle.out$special.stuff)
}
