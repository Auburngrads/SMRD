testthat::context("C++ GENMAX superalloy")

old.theta.hat  <- c(243.214474,-96.543583,9.667340,4.469588,-1.176405)
old.thetas.hat <- c( 3.9331473,-18.5287895,17.3157287,-0.9550840,-0.2257774)
old.correlation.matrix <- matrix(c( 1.0000000, -0.9998649,  0.9994660,  0.2805624, -0.2803016,
                                   -0.9998649,  1.0000000, -0.9998676, -0.2836626,  0.2834806,
                                    0.9994660, -0.9998676,  1.0000000,  0.2868757, -0.2867835,
                                    0.2805624, -0.2836626,  0.2868757,  1.0000000, -0.9991043,
                                   -0.2803016,  0.2834806, -0.2867835, -0.9991043,  1.0000000),
                                 nrow = 5, ncol = 5,byrow = T)

old.vcv.matrix <- matrix(c( 3365.61140, -1431.93256, 152.0588614,  67.929171, -14.5503634,
                           -1431.93256,   609.39450, -64.7296870, -29.224443,   6.2616505,
                             152.05886,   -64.72969,   6.8773871,   3.139789,  -0.6729488,
                              67.92917,   -29.22444,   3.1397890,  17.417662,  -3.7309718,
                             -14.55036,     6.26165,  -0.6729488,  -3.730972,   0.8006311),
                          nrow = 5, ncol = 5,byrow = T)

old.residuals <- c(-1.74889255,
                   -5.23742199,
                    0.08891754,
                   -1.77300429,
                   -2.93769217,
                    0.03580577,
                    0.30403316,
                    0.09270413,
                   -0.95850789,
                    0.25058231,
                   -0.66364026,
                    0.05243598,
                    0.54771328,
                    0.83233660,
                   -1.11647809,
                    0.74066585,
                    1.10643899,
                   -2.64659929,
                   -1.59055114,
                   -0.36674663,
                   -0.28791270,
                   -3.24423194,
                   -0.08138742,
                   -0.81161308,
                   -0.78300780,
                   -0.32964581)


data.ld <- SMRD::frame.to.ld(SMRD::superalloy,
                             response.column = 1,
                             censor.column = 2,
                             case.weight.column = 3,
                             x.columns = c(4,5,6))
new = SMRD::gmlest(data.ld,
                   distribution = "weibull",
                   explan.vars = list(mu.relat = c(2,3),
                                      sigma.relat = c(2)),
                   model = 0)

test_that(desc = "theta.hat", 
          code = expect_lt(max(abs(new$theta.hat - old.theta.hat)), 1e-1))

test_that(desc = "thetas.hat",
         code = expect_lt(max(abs(new$thetas.hat - old.thetas.hat)), 1e-1))

test_that(desc = "correlation matrix", 
          code = expect_lt(max(abs(new$correlation.matrix - old.correlation.matrix)), 1e-1))

test_that(desc = "vcv matrix",
         code = expect_lt(max(abs((new$vcv.matrix - old.vcv.matrix) / old.vcv.matrix)), 1e-1))

test_that(desc = "residuals", 
          code = expect_lt(max(abs(new$residuals - old.residuals)), 1e-1))