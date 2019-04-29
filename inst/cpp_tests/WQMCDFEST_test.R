library(smrdfortran)
test = 1
if(test == 1) {
data.ld <- frame.to.ld(heatexchanger,
                       response.column = c(1,2),
                       censor.column = 3,
                       case.weight.column = 4)
}
if(test == 2) data.ld <- frame.to.ld(lzbearing, response.column = 1)
if(test == 3) {
  
   data.ld <- frame.to.ld(superalloy,
                          response.column = 1,
                          censor.column = 2,
                          case.weight.column = 3)
   
}
if(test == 4) {
  
data.ld <- frame.to.ld(smrdfortran::doatrun,
                       response.column = c(1,2),
                       censor.column = 3,
                       case.weight.column = 4,
                       truncation.response.column = 5,
                       truncation.type.column = 6)

}
gamthr = 0
kprint = 0
maxit = 5e+05
tol = 0.001
maxmsd = 200
start.values = NULL # is this needed
debug1 = F

  y <- Response(data.ld)
  the.case.weights <- smrdfortran:::case.weights(data.ld)

  if (is.null(start.values)) nstart <- 0 # what if not null?
  number.cases <- nrow(y)
    ny <- ncol(y)
    the.censor.codes <- smrdfortran:::censor.codes(data.ld)

    if (length(gamthr) == 1)
      gamthr <- rep(gamthr, number.cases)

      if (length(gamthr) != number.cases)
        stop("specified offset is the wrong length")

        if (ny == 2)
          gamthr <- cbind(gamthr, gamthr)

          y <- y - as.matrix(gamthr)

          number.observations <- sum(the.case.weights[the.censor.codes > 0])
          left.trun.cond <- NULL
        right.trun.cond <- NULL
        the.truncation.codes <- smrdfortran:::truncation.codes(data.ld)

          if (is.null(the.truncation.codes)) {

            ty <- as.matrix(1)
            nty <- 0
            the.truncation.codes <- 1

          } else {

            ty <- smrdfortran:::truncation.response(data.ld)
            nty <- ncol(ty)
            if (all(the.truncation.codes == 3))
              left.trun.cond <- min(ty[the.truncation.codes == 3, 1])
              if (all(the.truncation.codes == 2))
                right.trun.cond <- max(ty[the.truncation.codes == 2, nty])
          }
dummy <- the.censor.codes
ndscrat <- 3 * number.cases + 4
nrscrat <- max(7 * (number.cases + 1),
               (maxmsd * (maxmsd - 1))/2 + 1)
niscrat <- 6 * number.cases + 7

 zout <- .Fortran("wqmcdfest", 
                     as.single(y), 
                     as.integer(ny),
                     as.single(the.censor.codes), 
                     as.single(the.case.weights),
                     as.single(ty), 
                     as.integer(nty), 
                     as.single(the.truncation.codes),
                     as.integer(number.cases), 
                     as.integer(nstart), 
                     double(ndscrat),
                     single(nrscrat), 
                     integer(niscrat), 
                     as.integer(kprint),
                     as.integer(maxit), 
                     as.single(tol), 
                     as.integer(maxmsd),
                     p = single(number.cases + 1), 
                     q = single(number.cases + 1), 
                     prob = single(number.cases + 1), 
                     sd = single(number.cases + 1), 
                     m = integer(1), 
                     pchmax = single(1), 
                     lsd = integer(1),
                     ier = integer(1))

new = wqmmlesss::wqmcdfest(y,
                   as.integer(ny),
                   as.integer(the.censor.codes),
                   as.integer(the.case.weights),
                   ty,
                   as.integer(nty),
                   as.integer(the.truncation.codes),
                   as.integer(number.cases),
                   as.integer(nstart),
                   double(ndscrat),
                   double(nrscrat),
                   integer(niscrat),
                   as.integer(kprint),
                   as.integer(maxit),
                   as.double(tol),
                   as.integer(maxmsd),
                   p = double(number.cases + 1),
                   q = double(number.cases + 1),
                   prob = double(number.cases + 1),
                   sd = double(number.cases + 1),
                   m = integer(1),
                   pchmax = double(1),
                   lsd = integer(1),
                   ier = integer(1))
