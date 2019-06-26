library(smrdfortran)

distribution  = "weibull"
kprint = 0
pi.vec = seq(0.001, 0.999, length = 100)
a = 1
b1 = 1
b2 = 0
pd = NULL
ph = NULL
theta = 1
theta.known = T
quad.model = F
perc = 0.01
zd = 0
levels = c(0.1,0.2, 0.5, 1, 2)
cex2 = 2
xlab = "X"
ylab = "Y"
scale.var = T
old.a = NULL
old.b1 = NULL
xiL.lower = 0.001
xi.vec = seq(xiL.lower,0.999, length = 100)
dump = F

    if (!is.null(ph) && !is.null(pd)) {
        a <- logb(-logb(1 - pd))
        b1 <- a - theta * logb(-logb(1 - ph))
        old.b1 <- -b1
        old.a <- a + old.b1
  } else {
        if (!is.null(old.a) && !is.null(old.b1)) {
            b1 <- -old.b1
            a <- old.a - old.b1
            pd <- psev(a)
            ph <- psev(old.a)
      } else {
            if (is.null(a)) stop("Need to specify a")
            old.b1 <- -1 * b1
            old.a <- a + old.b1
            pd <- psev(a)
            ph <- psev(old.a)
        }
    }
    if (dump) cat("pd=", pd, "ph=", ph, "\n")
    if (dump) cat("a =", a, "b1 =", b1, "    old.a =", old.a, "old.b1 =",
            old.b1, "\n")

b2 = 0; theta = 1; theta.known = T; quad.model = F
zd = 0

theta.in = 1
theta.known = T
quad.model = F
pi.in = pi.vec
xi.in = xi.vec

    det.matrix <- function (a) { return(prod(eigen(a)$values)) }
    nplan <- length(as.vector(pi.in))
    pi <- rbind(as.vector(pi.in), 1 - as.vector(pi.in))
    xi <- rbind(as.vector(xi.in), 1)
    a <- smrdfortran:::expand.vec(a, nplan)
    b1 <- smrdfortran:::expand.vec(b1, nplan)
    b2 <- smrdfortran:::expand.vec(b2, nplan)
    theta <- smrdfortran:::expand.vec(theta.in, nplan)
    perc <- smrdfortran:::expand.vec(perc, nplan)
    idist <- smrdfortran:::numdist(distribution)
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
    if (kprint > 2) browser()
    zout <- .Fortran("vvar1", as.double(parameter), as.double(xi),
        as.double(pi), as.double(zd), as.integer(dim), as.integer(nplan),
        as.integer(nrow(pi)), as.double(perc), as.integer(idistp),
        as.integer(known), fisher = double(dim * dim), varret = double(nplan),
        as.integer(kprint))
    
    new <- SMRD::vvar1(as.double(parameter), 
                            as.double(xi),
                            as.double(pi),
                            as.double(zd), 
                            as.integer(dim), 
                            as.integer(nplan),
                            as.integer(nrow(pi)), 
                            as.double(perc), 
                            as.integer(idistp),
                            as.integer(known), 
                            fret = matrix(0, nrow = dim, ncol = dim), 
                            varret = double(nplan),
                            as.integer(kprint))
