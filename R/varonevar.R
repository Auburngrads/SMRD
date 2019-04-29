varonevar <-
function (distribution, a, b1, b2 = 0, theta = 1, theta.known = T,
    quad.model = F, perc, xi, zd = 0, pi, kprint = 0)
{
    det.matrix <- function (a) { return(prod(eigen(a)$values)) }
    if (length(pi) != length(xi))
        stop("length(pi)!=length(xi)")
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
            type <- paste("Linear model with theta=", theta,
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
    variance.vec <- perc
    for (ivec in 1:length(perc)) {
        zout <- .Fortran("svar1", as.double(parameter), as.double(xi),
            as.double(pi), as.double(zd), as.integer(dim), as.integer(length(pi)),
            as.double(perc[ivec]), as.integer(idistp), as.integer(known),
            fisher = double(dim * dim), varret = double(1), as.integer(kprint))
        variance.vec[ivec] <- zout$varret
    }
    fisher <- matrix(zout$fisher, nrow = dim)
    vcv <- my.solve(fisher, tol = 1e-12)
    dimnames(fisher) <- list(par.names, par.names)
    dimnames(vcv) <- list(par.names, par.names)
    return(list(type = type, distribution = distribution, parameter = parameter,
        perc = perc, xi = xi, pi = pi, fisher = fisher, vcv = vcv,
        det.fisher = det.matrix(fisher), det.vcv = det.matrix(vcv),
        variance = variance.vec))
}
