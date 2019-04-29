vvaronevar <-
function (distribution, a, b1, b2 = 0, theta.in = 1, theta.known = T,
    quad.model = F, perc, xi.in, zd = 0, pi.in, kprint = 0)
{
    det.matrix <- function (a) { return(prod(eigen(a)$values)) }
    nplan <- length(as.vector(pi.in))
    pi <- rbind(as.vector(pi.in), 1 - as.vector(pi.in))
    xi <- rbind(as.vector(xi.in), 1)
    a <- expand.vec(a, nplan)
    b1 <- expand.vec(b1, nplan)
    b2 <- expand.vec(b2, nplan)
    theta <- expand.vec(theta.in, nplan)
    perc <- expand.vec(perc, nplan)
    idist <- numdist(distribution)
    idistp <- (idist + 1)/2
    parameter <- c(a, b1, b2, theta)
    if (theta.known) {
        if (quad.model) {
            dim <- 4
            type <- "Quadratic Model"
            par.names <- c(" beta0", " beta1", " beta2", " sigma")
            known <- 3
      } else {
            dim <- 3
            type <- paste("Linear model with theta=", theta.in,
                "known")
            par.names <- c(" beta0", " beta1", " sigma")
            known <- 2
        }
  } else {
        if (quad.model) {
            stop("With quadratic model, theta must be known")
      } else {
            dim <- 4
            type <- "Linear model with theta unknown"
            par.names <- c(" beta0", " beta1", " sigmad", "sigmah")
            known <- 1
        }
    }
    if (kprint > 2)
        browser()
    zout <- .Fortran("vvar1", as.double(parameter), as.double(xi),
        as.double(pi), as.double(zd), as.integer(dim), as.integer(nplan),
        as.integer(nrow(pi)), as.double(perc), as.integer(idistp),
        as.integer(known), fisher = double(dim * dim), varret = double(nplan),
        as.integer(kprint))
    if (nplan == 1) {
        fisher <- matrix(zout$fisher, nrow = dim)
        vcv <- my.solve(fisher, tol = 1e-12)
        dimnames(fisher) <- list(par.names, par.names)
        dimnames(vcv) <- list(par.names, par.names)
        return(list(type = type, distribution = distribution,
            parameter = parameter, perc = perc, xi = xi, pi = pi,
            fisher = fisher, vcv = vcv, det.fisher = det.matrix(fisher),
            det.vcv = det.matrix(vcv), variance = zout$varret))
  } else {
        return(zout$varret)
    }
}
