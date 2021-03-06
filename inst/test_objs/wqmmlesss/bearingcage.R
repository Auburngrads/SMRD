data.ld <-
structure(list(hours = c(50L, 150L, 230L, 250L, 334L, 350L, 423L, 
450L, 550L, 650L, 750L, 850L, 950L, 990L, 1009L, 1050L, 1150L, 
1250L, 1350L, 1450L, 1510L, 1550L, 1650L, 1850L, 2050L), event = structure(c(1L, 
1L, 2L, 1L, 2L, 1L, 2L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 1L, 1L, 
1L, 1L, 1L, 2L, 1L, 1L, 1L, 1L), .Label = c("Censored", "Failed"
), class = "factor"), count = c(288L, 148L, 1L, 124L, 1L, 111L, 
1L, 106L, 99L, 110L, 114L, 119L, 127L, 1L, 1L, 123L, 93L, 47L, 
41L, 27L, 1L, 11L, 6L, 1L, 2L)), class = c("life.data", "data.frame"
), row.names = c(NA, -25L), right.censor.names = "a,alive,c,censor,censored,end,mend,noreport,r,r-censored,right-censored,removed,right,rightcensored,s,survived,survive,suspend,suspended,2", left.censor.names = "l,l-censored,left-censored,left,leftcensored,start,mstart,3", interval.censor.names = "b,bin,i,interval,i-censored,intervalcensored,interval-censored,4", failure.censor.names = "event,exact,d,dead,died,f,fail,failed,failure,report,repair,repaired,replaced,replacement,1", sinterval.censor.names = "s,sinterval,smallinterval,small-interval,5", response.column = c(hours = "hours"), censor.column = c(event = "event"), case.weight.column = c(count = "count"), data.title = "bearingcage", time.units = "Hours", data.note = "")
debug1 <-
FALSE
distribution <-
"lognormal"
distribution.number <-
4
dummy <-
c(2, 2, 1, 2, 1, 2, 1, 2, 2, 2, 2, 2, 2, 1, 1, 2, 2, 2, 2, 2, 
1, 2, 2, 2, 2)
e <-
c(1e-04, 1e-04)
escale <-
10000
explan.vars <-
NULL
gamthr <-
c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
0, 0, 0, 0, 0)
int <-
1
intercept <-
TRUE
intercept.increment <-
1
ivec <-
c(number.case = 25, nter = 1, nyresp = 1, ntyresp = 0, distribution.number = 4, 
lcheck = 0, nparm = 2, int = 1, maxit = 500, kprint = 0, ierfit = 0, 
iervcv = 0)
kprint <-
0
likelihood.method <-
"smoothed"
mathsoft.gamthr <-
c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
0, 0, 0, 0, 0)
maxit <-
500
ndscrat <-
95
niscrat <-
6
non.pos.resp <-
c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, 
FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, 
FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
nparm <-
2
nter <-
1
ntyresp <-
0
number.cases <-
25L
nyresp <-
1L
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
6
the.case.weights <-
c(288L, 148L, 1L, 124L, 1L, 111L, 1L, 106L, 99L, 110L, 114L, 
119L, 127L, 1L, 1L, 123L, 93L, 47L, 41L, 27L, 1L, 11L, 6L, 1L, 
2L)
the.censor.codes <-
c(2, 2, 1, 2, 1, 2, 1, 2, 2, 2, 2, 2, 2, 1, 1, 2, 2, 2, 2, 2, 
1, 2, 2, 2, 2)
the.truncation.codes <-
c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
1, 1, 1, 1, 1)
the.xmat <-
structure(c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
1, 1, 1, 1, 1, 1, 1, 1), .Dim = c(25L, 1L))
theta.start <-
c(6.5350422591705, 0.897360633138867)
theta.start.comp <-
c(6.5350422591705, 0.897360633138867)
tyresp <-
c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
0, 0, 0, 0, 0)
yresp <-
structure(c(50, 150, 230, 250, 334, 350, 423, 450, 550, 650, 
750, 850, 950, 990, 1009, 1050, 1150, 1250, 1350, 1450, 1510, 
1550, 1650, 1850, 2050), .Dim = c(25L, 1L), .Dimnames = list(
    NULL, c(hours = "hours")))
zout <-
list(ivec = c(25L, 1L, 1L, 0L, 4L, 0L, 2L, 1L, 500L, 0L, 0L, 
0L), rvec = structure(c(0, 10000, -76.5879669189453), Csingle = TRUE), 
    number.cases = 25L, nparm = 2L, xmat = structure(c(1, 1, 
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
    1, 1, 1, 1), Csingle = TRUE), yresp = structure(c(3.91202306747437, 
    5.01063537597656, 5.43807935714722, 5.52146100997925, 5.81114101409912, 
    5.85793304443359, 6.04737234115601, 6.10924768447876, 6.30991840362549, 
    6.47697257995605, 6.62007331848145, 6.74523639678955, 6.85646200180054, 
    6.897705078125, 6.91671514511108, 6.95654535293579, 7.0475172996521, 
    7.13089895248413, 7.20785999298096, 7.27931880950928, 7.31986474990845, 
    7.34601020812988, 7.40853071212769, 7.52294111251831, 7.62559509277344
    ), Csingle = TRUE), structure(c(2, 2, 1, 2, 1, 2, 1, 2, 2, 
    2, 2, 2, 2, 1, 1, 2, 2, 2, 2, 2, 1, 2, 2, 2, 2, 288, 148, 
    1, 124, 1, 111, 1, 106, 99, 110, 114, 119, 127, 1, 1, 123, 
    93, 47, 41, 27, 1, 11, 6, 1, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), Csingle = TRUE), 
    tyresp = structure(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), Csingle = TRUE), 
    truncation.codes = structure(c(1, 1, 1, 1, 1, 1, 1, 1, 1, 
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1), Csingle = TRUE), 
    parameter.fixed = c(FALSE, FALSE), e = structure(c(1.0000000116861e-07, 
    1.0000000116861e-07), Csingle = TRUE), ndscrat = c(-0.2, 
    -0.2, -0.2, -0.2, -0.2, -0.2, -0.2, -0.2, -0.2, -0.2, -0.2, 
    -0.2, -0.2, -0.2, -0.2, -0.2, -0.2, -0.2, -0.2, -0.2, -0.2, 
    -0.2, -0.2, -0.2, -0.2, 5, 0, 5, 1, -1, 0, 0, 1, 10.7540530073295, 
    1.55426756993195, -53.7702650366477, 1.55426756993195, 3.25792248492269e-08, 
    3.0624188257906e-07, 1.58727727359962, 0.599613366328455, 
    0.599613366328455, 0.233858235270597, 39.6819318399905, -2.99806683164227, 
    -2.99806683164227, 0.233858235270597, -31.0937425279601, 
    0.441004418547329, -31.0937425279601, 0.441004418547329, 
    1.10769771789261e-07, 1.0000000116861e-07, 0.0106757624547456, 
    0.0086045889237154, 0.10375818332303, 0, 0, 0.116217058194374, 
    3.5527136788005e-15, 0, 0, 0, 0, -53.7702650366477, 1.55426756993195, 
    0.0753515202064694, 0.997157032970522, 0.997157032970522, 
    -0.0753515202064694, 330.686101340405, 3.5527136788005e-15, 
    -1.14925430283463e-15, 0.0605321823185091, 0.233858235270597, 
    0, -2.99806683164227, 39.6819318399905, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), niscrat = c(1L, 2L, 0L, 
    1L, 2L, 0L), theta.hat = structure(c(10.7540531158447, 1.55426752567291
    ), Csingle = TRUE), first.derivative = structure(c(3.25792264277425e-08, 
    3.06241872749524e-07), Csingle = TRUE), vcv.matrix = structure(c(1.58727729320526, 
    0.5996133685112, 0.5996133685112, 0.233858242630959), Csingle = TRUE), 
    correlation.matrix = structure(c(1, 0.984166502952576, 0.984166502952576, 
    1), Csingle = TRUE), residuals = structure(c(0.0122516741976142, 
    0.0248410757631063, 0.0327044688165188, 0.0345068760216236, 
    0.0415765307843685, 0.0428472422063351, 0.048401203006506, 
    0.0503669194877148, 0.0573082230985165, 0.0638109669089317, 
    0.0699649676680565, 0.075832225382328, 0.0814577713608742, 
    0.083648219704628, 0.084677591919899, 0.0868756100535393, 
    0.092112235724926, 0.097188726067543, 0.102122254669666, 
    0.106927014887333, 0.109753109514713, 0.11161495745182, 0.116196200251579, 
    0.125072136521339, 0.133611604571342), Csingle = TRUE), fitted.values.and.deviance = structure(c(46819.40234375, 
    46819.40234375, 46819.40234375, 46819.40234375, 46819.40234375, 
    46819.40234375, 46819.40234375, 46819.40234375, 46819.40234375, 
    46819.40234375, 46819.40234375, 46819.40234375, 46819.40234375, 
    46819.40234375, 46819.40234375, 46819.40234375, 46819.40234375, 
    46819.40234375, 46819.40234375, 46819.40234375, 46819.40234375, 
    46819.40234375, 46819.40234375, 46819.40234375, 46819.40234375, 
    -0.00154385296627879, -0.0162560846656561, -12.6470556259155, 
    -0.0471921637654305, -12.2279815673828, -0.0906167477369308, 
    -11.9924058914185, -0.148728638887405, -0.210381627082825, 
    -0.326431691646576, -0.446591973304749, -0.590633928775787, 
    -0.77406919002533, -11.3356637954712, -11.3244018554688, 
    -0.898376047611237, -0.79813951253891, -0.466385662555695, 
    -0.464151829481125, -0.344787150621414, -11.1208009719849, 
    -0.156923443078995, -0.0948252454400063, -0.0189941488206387, 
    -0.0446279980242252, 0.00711761321872473, 0.0639868676662445, 
    -3.42024350166321, 0.171141788363457, -3.18021941184998, 
    0.310304582118988, -3.02823066711426, 0.487032026052475, 
    0.663966357707977, 0.998226284980774, 1.32845163345337, 1.71417081356049, 
    2.19709897041321, -2.48113512992859, -2.46890425682068, 2.49863886833191, 
    2.17870950698853, 1.25121760368347, 1.22524404525757, 0.896456897258759, 
    -2.20952200889587, 0.402224630117416, 0.23980014026165, 0.0468509756028652, 
    0.107614949345589, 0.0313325710594654, 0.236475557088852, 
    1, 0.576401591300964, 1, 0.978362262248993, 1, 1.45769429206848, 
    1.90293955802917, 2.75600790977478, 3.54884386062622, 4.44593667984009, 
    5.54760980606079, 1, 1, 6.15562725067139, 5.24671268463135, 
    2.95002365112305, 2.83211326599121, 2.03389120101929, 1, 
    0.896665751934052, 0.525747835636139, 0.0995918437838554, 
    0.222399801015854), Csingle = TRUE))
