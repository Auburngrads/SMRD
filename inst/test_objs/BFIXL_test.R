test_cpp <- T
library(smrdfortran)
DATA <- lzbearing
lz.ld <- frame.to.ld(DATA, response.column = 1)

cdfest.out = cdfest(lz.ld)
band.type = 'pointwise'
conf.level = GetsmrdfortranDefault("smrdfortran.ConfLevel")/100
a.limit = 0.001
b.limit = 0.999
mono.tran = T

  cdpoints.out <- smrdfortran:::cdpoints(cdfest.out)
  ok <- cdpoints.out$sdplot > 0
  times <- cdpoints.out$yplot[ok]
  fhat.point <- cdpoints.out$pplot
  dist.probs <- cdpoints.out$pplot[ok]
  stderrq <- cdpoints.out$sdplot[ok]/(dist.probs[ok] * (1 -
    dist.probs[ok]))
  nux.squared <- cdpoints.out$number.observations * (cdpoints.out$sdplot[ok]/dist.probs)^2
  kx <- nux.squared/(1 + nux.squared)
  zvalue <- 0
  if (is.null(band.type) || band.type == "")
    band.type <- "none"
  switch(casefold(band.type), s = , simultaneous = {
  band.type <- "Simultaneous"
    bands.over <- kx > a.limit & kx < b.limit
    zvalue <- smrdfortran:::evalue(a = a.limit, b = b.limit, conf.level = conf.level)
  }, `Point-wise` = , pointwise = , p = , `point-wise` = {
    band.type <- "Pointwise"
    bands.over <- kx > 0 & kx < 1
    zvalue <- qnorm(1 - (1 - conf.level)/2)
  }, none = {
    band.type <- "none"
  }, {
    warning("band.type not recognized")
    band.type <- "none"
  })
    if (band.type == "none") {
      return(list(times = times, fhat = dist.probs, lower = NULL,
                  upper = NULL, bands.over = NULL, band.type = band.type))
    }
    lower <- plogis(qlogis(dist.probs) - zvalue * stderrq)
    upper <- plogis(qlogis(dist.probs) + zvalue * stderrq)
    loweR <- plogis(qlogis(dist.probs) - zvalue * stderrq)
    uppeR <- plogis(qlogis(dist.probs) + zvalue * stderrq)

            if (mono.tran) {
        loweR[!is.na(lower)] <- smrdfortran:::mono.lower(loweR[!is.na(loweR)])
        uppeR[!is.na(upper)] <- smrdfortran:::mono.upper(uppeR[!is.na(uppeR)])
            }


  lower[!is.na(lower)] <- BFIXL(lower[!is.na(lower)])
  upper[!is.na(upper)] <- BFIXU(upper[!is.na(upper)])


lower ; loweR
upper ; uppeR
