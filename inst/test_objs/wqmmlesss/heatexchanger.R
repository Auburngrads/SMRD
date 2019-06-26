data.ld <-
structure(list(lower = c(1L, 1L, 2L, 3L, 1L, 1L, 2L, 1L, 1L), 
    upper = c(1L, 2L, 3L, 3L, 1L, 2L, 2L, 1L, 1L), event = structure(c(2L, 
    1L, 1L, 3L, 2L, 1L, 3L, 2L, 3L), .Label = c("Interval", "Left", 
    "Right"), class = "factor"), count = c(1L, 2L, 2L, 95L, 2L, 
    3L, 95L, 1L, 99L), plant = structure(c(1L, 1L, 1L, 1L, 2L, 
    2L, 2L, 3L, 3L), .Label = c("Plant1", "Plant2", "Plant3"), class = "factor")), class = c("life.data", 
"data.frame"), row.names = c(NA, -9L), right.censor.names = "a,alive,c,censor,censored,end,mend,noreport,r,r-censored,right-censored,removed,right,rightcensored,s,survived,survive,suspend,suspended,2", left.censor.names = "l,l-censored,left-censored,left,leftcensored,start,mstart,3", interval.censor.names = "b,bin,i,interval,i-censored,intervalcensored,interval-censored,4", failure.censor.names = "event,exact,d,dead,died,f,fail,failed,failure,report,repair,repaired,replaced,replacement,1", sinterval.censor.names = "s,sinterval,smallinterval,small-interval,5", response.column = c(lower = "lower", 
upper = "upper"), censor.column = c(event = "event"), case.weight.column = c(count = "count"), data.title = "heatexchanger", time.units = c(lower = "lower"), data.note = "")
debug1 <-
FALSE
distribution <-
"lognormal"
distribution.number <-
4
dummy <-
c(3, 4, 4, 2, 3, 4, 2, 3, 2)
e <-
c(1e-04, 1e-04)
escale <-
10000
explan.vars <-
NULL
gamthr <-
c(0, 0, 0, 0, 0, 0, 0, 0, 0)
int <-
1
intercept <-
TRUE
intercept.increment <-
1
ivec <-
c(number.case = 9, nter = 1, nyresp = 2, ntyresp = 0, distribution.number = 4, 
lcheck = 0, nparm = 2, int = 1, maxit = 500, kprint = 0, ierfit = 0, 
iervcv = 0)
kprint <-
0
likelihood.method <-
"smoothed"
mathsoft.gamthr <-
c(0, 0, 0, 0, 0, 0, 0, 0, 0)
maxit <-
500
ndscrat <-
63
niscrat <-
6
non.pos.resp <-
c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE
)
nparm <-
2
nter <-
1
ntyresp <-
0
number.cases <-
9L
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
2
the.case.weights <-
c(1L, 2L, 2L, 95L, 2L, 3L, 95L, 1L, 99L)
the.censor.codes <-
c(3, 4, 4, 2, 3, 4, 2, 3, 2)
the.truncation.codes <-
c(1, 1, 1, 1, 1, 1, 1, 1, 1)
the.xmat <-
structure(c(1, 1, 1, 1, 1, 1, 1, 1, 1), .Dim = c(9L, 1L))
theta.start <-
c(0.276100738865333, 0.430373446356641)
theta.start.comp <-
c(0.276100738865333, 0.430373446356641)
tyresp <-
c(0, 0, 0, 0, 0, 0, 0, 0, 0)
yresp <-
structure(c(1, 1, 2, 3, 1, 1, 2, 1, 1, 1, 2, 3, 3, 1, 2, 2, 1, 
1), .Dim = c(9L, 2L), .Dimnames = list(NULL, c(lower = "lower", 
upper = "upper")))
zout <-
list(ivec = c(9L, 1L, 2L, 0L, 4L, 0L, 2L, 1L, 500L, 0L, 0L, 0L
), rvec = structure(c(0, 10000, -54.3504676818848), Csingle = TRUE), 
    number.cases = 9L, nparm = 2L, xmat = structure(c(1, 1, 1, 
    1, 1, 1, 1, 1, 1), Csingle = TRUE), yresp = structure(c(0, 
    0, 0.6931471824646, 1.0986123085022, 0, 0, 0.6931471824646, 
    0, 0, 0, 0.6931471824646, 1.0986123085022, 1.0986123085022, 
    0, 0.6931471824646, 0.6931471824646, 0, 0), Csingle = TRUE), 
    structure(c(3, 4, 4, 2, 3, 4, 2, 3, 2, 1, 2, 2, 95, 2, 3, 
    95, 1, 99, 0, 0, 0, 0, 0, 0, 0, 0, 0), Csingle = TRUE), tyresp = structure(c(0, 
    0, 0, 0, 0, 0, 0, 0, 0), Csingle = TRUE), truncation.codes = structure(c(1, 
    1, 1, 1, 1, 1, 1, 1, 1), Csingle = TRUE), parameter.fixed = c(FALSE, 
    FALSE), e = structure(c(1.0000000116861e-07, 1.0000000116861e-07
    ), Csingle = TRUE), ndscrat = c(-0.333333333333333, -0.333333333333333, 
    -0.333333333333333, -0.333333333333333, -0.333333333333333, 
    -0.333333333333333, -0.333333333333333, -0.333333333333333, 
    -0.333333333333333, 3, 0, 3, 1, -1, 0, 0, 1, 3.73756720766815, 
    1.69628575670357, -11.2127016230045, 1.69628575670357, 1.24973738259787e-07, 
    4.20105499659221e-07, 0.987501169878637, 0.518372074736796, 
    0.518372074736796, 0.287816370999144, 8.88751052890773, -1.55511622421039, 
    -1.55511622421039, 0.287816370999144, -11.2127016230045, 
    1.69628575670357, 0.172646240456142, 0.984983896140622, 0.984983896140622, 
    -0.172646240456142, 188.825364261219, 7.105427357601e-15, 
    2.38004060904018e-15, 0.314122003299614, 0.287816370999144, 
    0, -1.55511622421039, 8.88751052890773, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), niscrat = c(1L, 2L, 
    0L, 1L, 2L, 0L), theta.hat = structure(c(3.73756718635559, 
    1.69628572463989), Csingle = TRUE), first.derivative = structure(c(1.24973738024892e-07, 
    4.20105493503797e-07), Csingle = TRUE), vcv.matrix = structure(c(0.98750114440918, 
    0.518372058868408, 0.518372058868408, 0.287816375494003), Csingle = TRUE), 
    correlation.matrix = structure(c(1, 0.972333014011383, 0.972333014011383, 
    1), Csingle = TRUE), residuals = structure(c(0.11042894423008, 
    0.11042894423008, 0.16616789996624, 0.21103623509407, 0.11042894423008, 
    0.11042894423008, 0.16616789996624, 0.11042894423008, 0.11042894423008, 
    0.11042894423008, 0.16616789996624, 0.21103623509407, 0.21103623509407, 
    0.11042894423008, 0.16616789996624, 0.16616789996624, 0.11042894423008, 
    0.11042894423008), Csingle = TRUE), fitted.values.and.deviance = structure(c(41.9957008361816, 
    41.9957008361816, 41.9957008361816, 41.9957008361816, 41.9957008361816, 
    41.9957008361816, 41.9957008361816, 41.9957008361816, 41.9957008361816, 
    -4.28425550460815, -7.58294439315796, -7.49806976318359, 
    -5.86671304702759, -8.56851100921631, -11.3744163513184, 
    -3.51720643043518, -4.28425550460815, -1.37409651279449, 
    -2.55453038215637, -3.94341540336609, -3.33460187911987, 
    12.0197658538818, -5.10906076431274, -5.91512298583984, 7.85685157775879, 
    -2.55453038215637, 3.53464460372925, 0.897016882896423, 1.97321569919586, 
    1.9905709028244, 20.2202415466309, 1.79403376579285, 2.95982360839844, 
    14.7509269714355, 0.897016882896423, 7.91437482833862), Csingle = TRUE))
