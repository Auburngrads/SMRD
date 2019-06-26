objs = dir(system.file("test_objs","wqmmlesss", package = "SMRD"), 
           full.names = T)

for(i in seq_along(objs)) {
  
testthat::context(paste0("WQMMLESSS ", gsub(".R", "", basename(objs[i]))))

source(system.file("test_objs","wqmmlesss",basename(objs[i]), package = "SMRD"))

new = SMRD:::WQMMLESSS(  ivec = as.integer(ivec), 
                      rvec = as.double(rvec),
                      nrow = as.integer(number.cases), 
                      nparm = as.integer(nparm),
                      x = the.xmat, 
                      y = yresp,
                      cen = as.double(the.censor.codes), 
                      wt =  as.double(the.case.weights), 
                      msftgm = as.double(mathsoft.gamthr),
                      ty = as.matrix(tyresp), 
                      tcodes = as.double(the.truncation.codes),
                      lfix = as.logical(parameter.fixed), 
                      e = as.double(e),
                      dscrat = double(ndscrat), 
                      iscrat = integer(niscrat),
                      theta = as.double(theta.start), 
                      fsder = double(nparm),
                      vcv = matrix(0, nrow = nparm, ncol = nparm), 
                      r = matrix(0, nrow = nparm, ncol = nparm), 
                      res = matrix(0, nrow = number.cases, ncol = nyresp),
                      fv = double(number.cases),
                      dev = matrix(0, nrow = number.cases, ncol = 3),
                      ipxnew = matrix(0, nrow = number.cases, ncol = nter),
                      iprv1 = double(nparm),
                      ipdiag = double(nparm),
                      iptmat = matrix(0, nrow = nparm, ncol = nparm),
                      ipthb = double(nparm),
                      ipthg = double(nparm),
                      ipfsd = double(nparm),
                      ipvcvb = matrix(0, nrow = nparm, ncol = nparm),
                      ipvcvg = matrix(0, nrow = nparm, ncol = nparm),
                      ipnext = double(nparm),
                      itd = double(nparm),
                      itf = double(nparm),
                      ied = double(nparm),
                      iw = double(nparm * nparm + 3 * nparm),
                      ivd = double(nparm),
                      ivcvd = matrix(0, nrow = nparm, ncol = nparm),
                      ivcvdd = matrix(0, nrow = nparm + 1, ncol = nparm + 1),
                      iir = double(nparm + 1),
                      ijc = double(nparm + 1))

xnew.end = 0;
rv1.end = xnew.end + number.cases*nter
diag.end = rv1.end + nparm
tmat.end = diag.end + nparm
thetb.end = tmat.end + nparm * 2
thetg.end = thetb.end + nparm
fsder.end = thetg.end + nparm
vcv.end = fsder.end + nparm
vcvg.end = vcv.end + nparm ** 2
itd.end = vcvg.end + nparm ** 2
itf.end = itd.end + nparm
ied.end = itf.end + nparm
iw.end = ied.end + nparm
ivd.end = iw.end + nparm*nparm+3*nparm
ivcvd.end = ivd.end + nparm
ivcvdd.end = ivcvd.end + nparm ** 2

test_that(desc = "xnew", 
          code = expect_lt(max(abs((zout$ndscrat[(xnew.end + 1):(number.cases*nter)] - new$nummat$ipxnew[1:(number.cases*nter)])/zout$ndscrat[(xnew.end + 1):(number.cases*nter)])), 1e-2 ))

test_that(desc = "rv1",
         code = expect_lt(max(abs(zout$ndscrat[(rv1.end + 1):(rv1.end + nparm)] - new$numvec$iprv1)),1e-4))

test_that(desc = "diag",
         code = expect_lt(max(abs((zout$ndscrat[(diag.end + 1):(diag.end + nparm)] - new$numvec$ipdiag) / zout$ndscrat[(diag.end + 1):(diag.end + nparm)])),1e-2))

test_that(desc = "tmat",
         code = expect_lt(max(abs(zout$ndscrat[(tmat.end + 1):(tmat.end + nparm * 2)] - new$nummat$iptmat[1:(nparm * 2)])),1e-2))

test_that(desc = "thetb",
         code = expect_lt(max(abs((zout$ndscrat[(thetb.end + 1):(thetb.end + nparm)] - new$numvec$ipthb) / zout$ndscrat[(thetb.end + 1):(thetb.end + nparm)])),1e-2))

test_that(desc = "thetg",
         code = expect_lt(max(abs((zout$ndscrat[(thetg.end + 1):(thetg.end + nparm)] - new$numvec$ipthg) / zout$ndscrat[(thetg.end + 1):(thetg.end + nparm)])), 1e-2))

test_that(desc = "fsder",
         code = expect_lt(max(abs(zout$ndscrat[(fsder.end + 1):(fsder.end + nparm)] - new$numvec$fsder)), 1e-4))

test_that(desc = "vcv",
         code = expect_lt(max(abs(zout$ndscrat[(vcv.end + 1):(vcv.end + nparm ** 2)] - new$nummat$vcv[1:(nparm ** 2)])), 1e-4))

test_that(desc = "vcvg",
         code = expect_lt(max(abs(zout$ndscrat[(vcvg.end + 1):(vcvg.end + nparm ** 2)] - new$nummat$ipvcvg[1:(nparm ** 2)])), 1e-4))

test_that(desc = "itd",
         code = expect_lt(max(abs(zout$ndscrat[(itd.end + 1):(itd.end + nparm)] - new$numvec$itd)), 1e-4))

test_that(desc = "itf",
         code = expect_lt(max(abs(zout$ndscrat[(itf.end + 1):(itf.end + nparm)] - new$numvec$itf)), 1e-4))

test_that(desc = "ied",
         code = expect_lt(max(abs(zout$ndscrat[(ied.end + 1):(ied.end + nparm)] - new$numvec$ied)), 1e-4))

test_that(desc = "iw",
         code = expect_lt(max(abs(zout$ndscrat[(iw.end + 1):(iw.end + nparm*nparm+3*nparm)] - new$numvec$iw)), 1e-4))

test_that(desc = "ivd",
         code = expect_lt(max(abs(zout$ndscrat[(ivd.end + 1):(ivd.end + nparm)] - new$numvec$ivd)),1e-2))

test_that(desc = "ivcvd",
         code = expect_lt(max(abs(zout$ndscrat[(ivcvd.end + 1):(ivcvd.end + nparm ** 2)] - new$nummat$ivcvd[1:(nparm ** 2)])), 1e-2))
         
test_that(desc = "ivcvdd",
         code = expect_lt(max(abs(zout$ndscrat[(ivcvdd.end + 1):(ivcvdd.end + (nparm + 1) ** 2)] - new$nummat$ivcvdd[1:((nparm + 1) ** 2)])),1e-2))

test_that(desc = "theta",
         code = expect_lt(max(abs(zout$theta - new$numvec$theta)),1e-2))

test_that(desc = "deviance",
         code = expect_lt(max(abs(zout$fitted[-(1:number.cases)] - new$nummat$dev)),1e-2))

test_that(desc = "fitted values",
         code = expect_lt(max(abs(zout$fitted[1:number.cases] - new$numvec$fv)),1e-2))

test_that(desc = "resids",
         code = expect_lt(max(abs(zout$residuals - new$nummat$res)), 1e-2))

test_that(desc = "correl",
         code = expect_lt(max(abs(zout$correl - new$nummat$r)), 1e-2))

test_that(desc = "yresp",
         code = expect_lt(max(abs(zout$yresp - new$nummat$y)), 1e-2))

test_that(desc = "xlike",
         code = expect_lt(max(abs(zout$rvec[3] - new$doubs$xlike)), 1e-2))

test_that(desc = "ierfit",
         code = expect_equal(zout$ivec[11], new$ints$ierfit))

test_that(desc = "iervcv",
         code = expect_equal(zout$ivec[12], new$ints$iervcv))

}