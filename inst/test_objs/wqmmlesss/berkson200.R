data.ld <-
structure(list(lower = c(100L, 100L, 300L, 500L, 700L, 1000L, 
2000L, 4000L), upper = c(100L, 300L, 500L, 700L, 1000L, 2000L, 
4000L, 4000L), event = structure(c(2L, 1L, 1L, 1L, 1L, 1L, 1L, 
3L), .Label = c("interval", "left", "right"), class = "factor"), 
    count = c(41L, 44L, 24L, 32L, 29L, 21L, 9L, 0L)), class = c("life.data", 
"data.frame"), row.names = c(NA, -8L), right.censor.names = "a,alive,c,censor,censored,end,mend,noreport,r,r-censored,right-censored,removed,right,rightcensored,s,survived,survive,suspend,suspended,2", left.censor.names = "l,l-censored,left-censored,left,leftcensored,start,mstart,3", interval.censor.names = "b,bin,i,interval,i-censored,intervalcensored,interval-censored,4", failure.censor.names = "event,exact,d,dead,died,f,fail,failed,failure,report,repair,repaired,replaced,replacement,1", sinterval.censor.names = "s,sinterval,smallinterval,small-interval,5", response.column = c(lower = "lower", 
upper = "upper"), censor.column = c(event = "event"), case.weight.column = c(count = "count"), data.title = "berkson200", time.units = "1/5000 Seconds", data.note = "")
debug1 <-
FALSE
distribution <-
"lognormal"
distribution.number <-
4
dummy <-
c(3, 4, 4, 4, 4, 4, 4, 2)
e <-
c(1e-04, 1e-04)
escale <-
10000
explan.vars <-
NULL
gamthr <-
c(0, 0, 0, 0, 0, 0, 0, 0)
int <-
1
intercept <-
TRUE
intercept.increment <-
1
ivec <-
c(number.case = 8, nter = 1, nyresp = 2, ntyresp = 0, distribution.number = 4, 
lcheck = 0, nparm = 2, int = 1, maxit = 500, kprint = 0, ierfit = 0, 
iervcv = 0)
kprint <-
0
likelihood.method <-
"smoothed"
mathsoft.gamthr <-
c(0, 0, 0, 0, 0, 0, 0, 0)
maxit <-
500
ndscrat <-
61
niscrat <-
6
non.pos.resp <-
c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
nparm <-
2
nter <-
1
ntyresp <-
0
number.cases <-
8L
nyresp <-
2L
oldClasses <-
"life.data"
param.names <-
c("mu", "sigma")
parameter.fixed <-
c(FALSE, FALSE)
regression <-
FALSE
relationship <-
NULL
rvec <-
c(0, escale = 10000, log.likelihood = 0)
startna <-
c(TRUE, TRUE)
test <-
4
the.case.weights <-
c(41L, 44L, 24L, 32L, 29L, 21L, 9L, 0L)
the.censor.codes <-
c(3, 4, 4, 4, 4, 4, 4, 2)
the.truncation.codes <-
c(1, 1, 1, 1, 1, 1, 1, 1)
the.xmat <-
structure(c(1, 1, 1, 1, 1, 1, 1, 1), .Dim = c(8L, 1L))
theta.start <-
c(6.31031483234053, 1.32059516535914)
theta.start.comp <-
c(6.31031483234053, 1.32059516535914)
tyresp <-
c(0, 0, 0, 0, 0, 0, 0, 0)
yresp <-
structure(c(100, 100, 300, 500, 700, 1000, 2000, 4000, 100, 300, 
500, 700, 1000, 2000, 4000, 4000), .Dim = c(8L, 2L), .Dimnames = list(
    NULL, c(lower = "lower", upper = "upper")))
zout <-
list(ivec = c(8L, 1L, 2L, 0L, 4L, 0L, 2L, 1L, 500L, 0L, 0L, 0L
), rvec = structure(c(0, 10000, -387.884704589844), Csingle = TRUE), 
    number.cases = 8L, nparm = 2L, xmat = structure(c(1, 1, 1, 
    1, 1, 1, 1, 1), Csingle = TRUE), yresp = structure(c(4.60517024993896, 
    4.60517024993896, 5.70378255844116, 6.21460819244385, 6.55108022689819, 
    6.90775537490845, 7.60090255737305, 4000, 4.60517024993896, 
    5.70378255844116, 6.21460819244385, 6.55108022689819, 6.90775537490845, 
    7.60090255737305, 8.29404926300049, 4000), Csingle = TRUE), 
    structure(c(3, 4, 4, 4, 4, 4, 4, 2, 41, 44, 24, 32, 29, 21, 
    9, 0, 0, 0, 0, 0, 0, 0, 0, 0), Csingle = TRUE), tyresp = structure(c(0, 
    0, 0, 0, 0, 0, 0, 0), Csingle = TRUE), truncation.codes = structure(c(1, 
    1, 1, 1, 1, 1, 1, 1), Csingle = TRUE), parameter.fixed = c(FALSE, 
    FALSE), e = structure(c(1.0000000116861e-07, 1.0000000116861e-07
    ), Csingle = TRUE), ndscrat = c(-0.353553390593274, -0.353553390593274, 
    -0.353553390593274, -0.353553390593274, -0.353553390593274, 
    -0.353553390593274, -0.353553390593274, -0.353553390593274, 
    2.82842712474619, 0, 2.82842712474619, 1, -1, 0, 0, 1, 5.77931748678281, 
    1.20283656584596, -16.3463783421365, 1.20283656584596, -2.74846399724912e-07, 
    3.50332553116299e-08, 0.00786355624030814, -0.000844770833843394, 
    -0.000844770833843394, 0.00528247276509137, 0.0629084499224652, 
    0.00238937274063711, 0.00238937274063711, 0.00528247276509137, 
    -16.3463783421365, 1.20283656584596, -0.0413570112119219, 
    0.999144432814204, 0.999144432814204, 0.0413570112119219, 
    279.1156619355, -1.77635683940025e-15, 1.11022302462516e-16, 
    22.9626505723828, 0.00528247276509137, 0.00130052070892322, 
    0.00238937274063711, 0.0629084499224652, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), niscrat = c(1L, 2L, 0L, 
    1L, 2L, 0L), theta.hat = structure(c(5.7793173789978, 1.20283651351929
    ), Csingle = TRUE), first.derivative = structure(c(-2.74846399861417e-07, 
    3.50332562959466e-08), Csingle = TRUE), vcv.matrix = structure(c(0.007863556034863, 
    -0.00084477080963552, -0.00084477080963552, 0.00528247281908989
    ), Csingle = TRUE), correlation.matrix = structure(c(1, -0.131072252988815, 
    -0.131072252988815, 1), Csingle = TRUE), residuals = structure(c(0.376759350299835, 
    0.376759350299835, 0.939133763313293, 1.43603646755219, 1.89955365657806, 
    2.55524301528931, 4.54672765731812, 1.00000001504747e+30, 
    0.376759350299835, 0.939133763313293, 1.43603646755219, 1.89955365657806, 
    2.55524301528931, 4.54672765731812, 8.09031677246094, 1.00000001504747e+30
    ), Csingle = TRUE), fitted.values.and.deviance = structure(c(323.538299560547, 
    323.538299560547, 323.538299560547, 323.538299560547, 323.538299560547, 
    323.538299560547, 323.538299560547, 323.538299560547, -73.9997787475586, 
    -51.4655990600586, -43.0525245666504, -74.2776641845703, 
    -70.9887771606445, -46.5208473205566, -27.5795116424561, 
    0, -61.7487106323242, -21.3170757293701, 3.53545832633972, 
    15.9517002105713, 22.739408493042, 25.052734375, 15.7864847183228, 
    0, 32.7217216491699, 41.0569343566895, 23.6415214538574, 
    31.7920799255371, 28.788703918457, 20.4390621185303, 8.76617813110352, 
    0), Csingle = TRUE))
