#' Adhesive bonded power element test
#' 
#' @docType data
#' @name adhesivebondc
#' @family data-notdone
#' @format A \code{data.frame} with 336 rows and 4 variables:
#' \tabular{rlll}{
#'  [, 1] \tab celsius  \tab Temperature applied to the bond \tab \bold{Numeric}\cr
#'  [, 2] \tab rh       \tab Relative humidity applied to the bond \tab \bold{Numeric}\cr
#'  [, 3] \tab days     \tab Time the bond was under load \tab \bold{Numeric}\cr
#'  [, 4] \tab pounds   \tab Load at which the bond failed \tab \bold{Numeric}
#'  }
#' @source ####
#' @description How do pounds and days work together? Two responses? Constant strain?
NULL

#' Accelerated adhesive creep test
#'
#' @docType data
#' @name adhesivestrength
#' @family data-notdone
#' @format A \code{data.frame} with 89 rows and 4 variables:
#' \tabular{rlll}{
#'   [, 1] \tab pounds \tab Load at which the adhesive failed \tab \bold{Numeric} \cr
#'   [, 2] \tab celsius \tab Temperature applied to the bond \tab \bold{Numeric}\cr
#'   [, 3] \tab days \tab Time the bond was under load \tab \bold{Numeric}\cr
#'   [, 4] \tab group \tab Type of adhesive\tab \bold{Categoric}
#'   }
#' @source Beckwith, J.P. (1980), An estimator and design techinque for the estimation of a rate parameter in accelerated testing.  Department of Mathematics and Computer Science, Michigan Technological University, Houghton, MI.
#' @description ####
NULL

#' Fatigue crack growth in an unknown metallic alloy.
#' 
#' @docType data
#' @name alloya
#' @format  A \code{data.frame} with 262 rows and 3 variables
#' \tabular{rlll}{
#'   [, 1] \tab inches \tab Crack length observed at \code{megacycles} \tab \bold{Numeric}\cr
#'   [, 2] \tab specimen \tab Test specimen designator \tab \bold{Categoric}\cr 
#'   [, 3] \tab megacycles \tab Number of fatigue cycles (in millions) when \code{length} was inspected  \tab \bold{Numeric}
#'   }
#' @source Hudak, S.J., Saxena, A., Bucci, R. J., and Malcom, R .C. (1978), 
#'         Development of standard methods of testing and analyzing fatigure crack growth rate data, 
#'         Technical Report AFML-TR-78-40, Westinghouse R & D Center, Westinghouse Electric Corporation, Pittsburgh, PA.
#' @source Bogdanoff, J. L. and Kozin, F. (1985), Probablistic Models of Cumulative Damage, New York, NY; John Wiley & Sons. 
#' @description Twenty-one specimens of an unknown alloy were subjected 
#'              to the same cyclic load profile.  Prior to testing, an initial crack 
#'              (length = 0.9 in) was cut into each specimen to serve as a crack 
#'              nucleation site.  After each increment of 10,000 fatigue cycles the length
#'              of the crack was recorded.  A specimen was considered to have failed when
#'              the crack length exceeded 1.6 inches, otherwise the test concluded after 1.2 million 
#'              cycles.
NULL

#' Metal alloy tensile strength test 
#' 
#' @docType data
#' @name alloyc
#' @format A \code{data.frame} with 9 rows and 4 variables:
#' \tabular{rlll}{
#'   [, 1] \tab ksil   \tab Lower limit of the stress interval \tab \bold{Numeric}\cr
#'   [, 2] \tab ksiu   \tab Upper limit of the stress interval \tab \bold{Numeric}\cr
#'   [, 3] \tab event  \tab Event observed in the interval (interval-censored)\tab \bold{Categoric}\cr
#'   [, 4] \tab count  \tab Number of events observed in the interval\tab \bold{Numeric}
#'   }
#' @source Meeker W.Q. and Escobar L.A., Statistical Methods for Reliability Data, New York, NY; John Wiley & Sons. \bold{276}
#' @description These data resulted from a test that was conducted to obtain information on the strength of an alloy produced by 
#'              an alternate process.  The designation for the alloy was not provided and is only referred to as "Alloy C".  Specimens
#'              of the alloy were subjected to different levels of constant stress, measured in ksi (1 ksi = 1000 psi).  The data set 
#'              includes the number of failed specimens observed in each interval of applied stress. 
NULL

#' AMSAA reliability growth test
#' 
#' @rdname AMSAA
#' @docType data
#' @family data-notdone
#' @name amsaaexactfail
#' @name amsaawindow1
#' @name amsaawindow2  
#' @format A \code{data.frame} with 725 rows and 3 variables:
#' \tabular{rlll}{
#'   [, 1] \tab vehicle \tab Vehicle type \tab \bold{Categoric}\cr
#'   [, 2] \tab miles   \tab Accumulated distance at \code{event} \tab \bold{Numeric}\cr 
#'   [, 3] \tab event   \tab Event observed at \code{miles} (Start/Fail/End) \tab \bold{Categoric}
#'   }
#' @rdname AMSAA
#' @source Lieblein J., and Zelen, M. (1956), Statistical investigation of the fatigue life of deep-groove ball bearings, Journal of Research,  National Bureau of Standards, 57, 273--316.
#' @description ####
NULL

#' Appliance cord failures
#'
#' @docType data
#' @family data-notdone
#' @name appliancea 
#' @format A \code{data.frame} with 36 rows and 3 variables:
#' \tabular{rlll}{
#'   [, 1] \tab hours  \tab Accumulated time at \code{event} \tab \bold{Numeric}\cr
#'   [, 2] \tab event  \tab Event observed at \code{hours} (failed/right-censored) \tab \bold{Categoric}\cr 
#'   [, 3] \tab type   \tab Appliance type \tab \bold{Categoric}
#'   }
#' @source Nelson W. (1982), Applied Life Data Analysis, New York: John Wiley & Sons, pg 121
#' @description ####
NULL

#' Appliance-B failures (multiple data sources)
#'
#' @docType data
#' @family data-notdone
#' @name applianceb
#' @format A \code{data.frame} with 398 rows and 5 variables:
#' \tabular{rlll}{
#'   [, 1] \tab source \tab Information source (test environment) \tab \bold{Categoric}\cr
#'   [, 2] \tab event \tab Event observed at \code{days} (failure/right-censored/left-censored/interval-censored)  \tab \bold{Categoric}\cr
#'   [, 3] \tab count \tab Number of events observed at \code{time} \tab \bold{Numeric} \cr
#'   [, 4] \tab days \tab Accumulated time at \code{event} \tab \bold{Numeric}\cr 
#'   [, 5] \tab mode \tab Failure mode observed at \code{days} \tab \bold{Categoric}
#'   }
#' @source ####
#' @description ####
NULL

#' Herbicide concentration test
#'
#' @docType data
#' @family data-notdone
#' @name atrazinejune 
#' @format A \code{data.frame} with 24 rows and 2 variables:
#' \tabular{rlll}{
#'   [, 1] \tab percent \tab Atrazine concentration \tab \bold{Numeric}\cr
#'   [, 2] \tab event \tab Event observed at \code{percent} (failed/left-censored)  \tab \bold{Categoric}
#'   }
#' @source ####
#' @description ####
NULL

#' #####
#'
#' @docType data
#' @family data-notdone
#' @name aprel72
#' @format A \code{data.frame} with 25 rows and 5 variables:
#' \tabular{rlll}{
#'   [, 1] \tab hoursl \tab Start of observation interval \tab \bold{Numeric}\cr
#'   [, 2] \tab hoursu  \tab End of observation interval   \tab \bold{Numeric}\cr 
#'   [, 3] \tab event  \tab Event observed in the interval (right-censored/left-censored/interval-censored)  \tab \bold{Categoric}\cr
#'   [, 4] \tab count \tab Number of events observed in the interval \tab \bold{Numeric} \cr
#'   [, 5] \tab celsius  \tab Temperature applied \tab \bold{Numeric}
#'   }
#' @source Meeker W.Q. and Escobar L.A. (1998), Statistical Methods for Reliability Data 
#' @description ####
NULL

#' Fatigue life test (alloy T7987)
#' 
#' @docType data
#' @name at7987
#' @format A \code{data.frame} with 68 rows and 3 variables:
#' \tabular{rlll}{
#'   [, 1] \tab kilocycles \tab Accumulated cycles at \code{event} (in thousands) \tab \bold{Numeric}\cr
#'   [, 2] \tab event \tab Event observed at \code{kilocycles} (failed/right-censored) \tab \bold{Categoric}\cr 
#'   [, 3] \tab count \tab Number of events observed at \code{kilocycles} \tab \bold{Numeric}
#'   }
#' @source Meeker W.Q. and Escobar L.A. (1998) Statistical Methods for Reliability Data, New York, NY; John Wiley & Sons.
#' @description A test was conducted to assess the fatigue life of an alloy, 
#'              designated as T-7987. In this test, 72 specimens were subjected 
#'              to an unreported load spectrum. The number of cycles at which a 
#'              specimen failed was recorded (rounded to the nearest thousand cycles).
#'              Of the 72 units tested, 67 units failures were observed and 5 units were right 
#'              censored at 300 kilocycles.
NULL

#' \eqn{\alpha}-particle emissions of Americurium-241
#'
#' @docType data
#' @name berkson20
#' @name berkson200
#' @name berkson2000
#' @name berkson10220
#' @rdname berkson    
#' @format A \code{data.frame} with 8 rows and 4 variables:
#' \tabular{rlll}{
#'   [, 1] \tab lower \tab Start of an observation interval (in 1/5000 seconds) \tab \bold{Numeric}\cr
#'   [, 2] \tab upper \tab End of an observation interval (in 1/5000 seconds) \tab \bold{Numeric}\cr
#'   [, 3] \tab event \tab Event observed in the interval (right-censored/left-censored/interval-censored)  \tab \bold{Categoric}\cr
#'   [, 4] \tab count \tab Number of events observed in the interval \tab \bold{Numeric}
#'    }
#' @source Berkson, J. (1966), Examination of randomness of alpha-particle emissions, 
#'         in Festschrift for J. Neyman, Research Papers in Statistics, F. N. David, Editor, 
#'         New York, NY; John Wiley & Sons
#' @description Berkson investigated the randomness of alpha-particle 
#'              emissions of Americium-241 (which has a half-life of about 458 years). 
#'              Physical theory suggests that, over a short period of time, the 
#'              interarrival times of observed particles would be independent and come 
#'              from an exponential distribution where the rate parameter is the mean 
#'              time between arrivals. The corresponding homogeneous Poisson process that 
#'              counts the number of emissions on the real-time line has arrival rate or 
#'              the intensity lambda=1/theta. The data consist of 10,220 observed interarrival 
#'              times of alpha particles (time unit equal to 1/5,000 second). The observed 
#'              interarrival times were put into intervals (or bins) running from 0 to 4,000 
#'              time units with interval lengths ranging from 25 to 100 time units, with one 
#'              additional interval for observed times exceeding 4,000 time units.  To save space, 
#'              this example uses a smaller number of larger bins; reducing the number of bins in 
#'              this way will not seriously affect the precision of ML estimates.
NULL

#' Yokobori fatigue-fracture test
#' 
#' @docType data
#' @name bkfatigue10   
#' @format A \code{data.frame} with 63 rows and 1 variable:
#' \tabular{rlll}{
#'   [, 1] \tab kilocycles \tab Accumulated cycles at failure (in thousands) \tab \bold{Numeric}
#'    }
#' @source Bogdanoff, J.,L. and Kozin, F. (1985) Probablistic Models of Cumulative Damage, pp. 224-225, 
#'         New York, NY; John Wiley & Sons.
#' @source Yokobori, T. (1951) Fatigue fracture in steel, Journal of the Physical Society of Japan, \bold{6}, 81-86.
#' @description Yokobori describes a fatigue-fracture test on 0.41\% carbon steel cylindrical specimens, tested at 
#'              \eqn{\pm 37.1 kg/mm^2} stress amplitude.
NULL

#' Bleed system reliability data 
#'
#' @docType data
#' @name bleed
#' @format A \code{data.frame} with 60 rows and 4 variables:
#' \tabular{rlll}{
#'   [, 1] \tab hours \tab Accumulated time at \code{event} \tab \bold{Numeric}\cr
#'   [, 2] \tab event \tab Event observed at \code{hours} (failure/right-censored/left-censored/interval-censored)  \tab \bold{Categoric}\cr
#'   [, 3] \tab count \tab Number of events observed at \code{hours} \tab \bold{Numeric}\cr
#'   [, 4] \tab base  \tab Base where observation was made \tab \bold{Categoric}
#'    }
#' @source Abernethy, R. B., Breneman, J. E., Medlin, C. H., and Reinman, G. L., (1983) 
#'         Weibull Analysis Handbook, Air Force Wright Aeronautical Laboratories Technical Report AFWAL-TR-83-2079
#' @description Abernethy, Breneman, Medlin, and Reinman give failure and 
#'              running times for 2256 bleed systems operating at several 
#'              geographically separated bases. They observed a change in 
#'              the probability plot before and after 600 of operation.  
#'              Further examination showed that 9 of the 19 failures occurred 
#'              at Base 'D'.  Separate analyses of the Base D data and the data 
#'              from the other bases indicated different life distributions. 
#'              The large slope for Base D indicated strong wearout behavior. 
#'              The relatively small slope for the other bases suggested infant 
#'              mortality or accidental failures. After investigation it was 
#'              determined that the early-failure problem at base D was caused by 
#'              salt air (Base D was near the ocean). A change in maintenance 
#'              procedures there solved the dominant bleed system reliability problem.
NULL

#' Light bulb failure test 
#'
#' @docType data
#' @family data-notdone-source-desc
#' @name bulb
#' @format A \code{data.frame} with 417 rows and 1 variable:
#' \tabular{rlll}{
#'   [, 1] \tab hours \tab Accumulated time at failure \tab \bold{Numeric}
#'    }
#' @source Davis, D. J. (1952), An analysis of some failure data, Journal of the American Statistical Association, 47,113-150.
#' @description ####
NULL

#' Large system bearing cage fracture test 
#'
#' @docType data
#' @name bearingcage
#' @description Ball bearing assembly failure data   
#' @format A \code{data.frame} with 25 rows and 3 variables:
#' \tabular{rlll}{
#'   [, 1] \tab hours \tab Accumulated time at \code{event} \tab \bold{Numeric}\cr
#'   [, 2] \tab event \tab Event observed at \code{hours} (failure/right-censored/left-censored/interval-censored)  \tab \bold{Categoric}\cr
#'   [, 3] \tab count \tab Number of events observed at \code{hours} \tab \bold{Numeric}
#'    }
#' @source Abernethy, R. B., Breneman, J. E., Medlin, C. H., and Reinman, G. L. (1983) 
#'         Weibull Analysis Handbook, Air Force Wright Aeronautical Laboratories Technical Report AFWAL-TR-83-2079
#' @description The service life requirement for a ball bearing assembly was specified 
#'          such that \eqn{t_{0.1}}{t[0.1]} (aka the B10 life) be greater that 8000 hours. 
#'          Analysts were concerned that the design of the bearing cage in this assembly 
#'          was inadequate and could lead to premature failures during service.
#'          Service times were collected for 1703 assemblies that were introduced into 
#'          service over time.  The analysts wanted to use the service life data to 
#'          determine if a redesign was needed to ensure that the units could meet the 
#'          service life requirement.  Management was also interested in determining the 
#'          number of additional failures that could be expected over the next year
#'          for the population of assemblies already in service.
#' @details Bearing cages are used in ball bearing assemblies to ensure that the 
#'          ball bearings do not drift out of position relative to the other ball 
#'          bearings during use.
NULL

#' #####
#' 
#' @docType data
#' @family data-notdone
#' @name bearinga
#' @format A \code{data.frame} with 5 rows and 3 variables:
#' \tabular{rlll}{
#'   [, 1] \tab kilocycles \tab Accumulated cycles at \code{event} (in thousands) \tab \bold{Numeric}\cr
#'   [, 2] \tab event      \tab Event observed at \code{kilocycles} (failure/right-censored/left-censored/interval-censored)  \tab \bold{Categoric}\cr
#'   [, 3] \tab count      \tab Number of events observed at \code{kilocycles} \tab \bold{Numeric}
#'    }
#' @source ####
#' @description ####
NULL

#' ##### 
#' 
#' @docType data
#' @family data-notdone
#' @name censortruncationtest   
#' @format A \code{data.frame} with 16 rows and 6 variables:
#' \tabular{rlll}{
#'   [, 1] \tab lower   \tab Start of an observation interval in hours \tab \bold{Numeric}\cr
#'   [, 2] \tab upper  \tab End of an observation interval in hours \tab \bold{Numeric}\cr
#'   [, 3] \tab event \tab Event observed in the interval (failure/right-censored/left-censored/interval-censored)  \tab \bold{Categoric}\cr
#'   [, 4] \tab trun_lower  \tab Start of a truncation interval in hours \tab \bold{Numeric}\cr
#'   [, 5] \tab trun_upper \tab End of a truncation interval in hours \tab \bold{Numeric}\cr
#'   [, 6] \tab trun_event  \tab Truncation type observed (right/left/interval/exact) \tab \bold{Categoric}
#'    }
#' @source ####
#' @description ####
NULL

#' Rolling contact fatigue of ceramic ball bearings
#' 
#' @docType data
#' @name ceramicbearing   
#' @format A \code{data.frame} with 69 rows and 5 variables:
#' \tabular{rlll}{
#'   [, 1] \tab kilocycles \tab Accumulated cycles at \code{event} (in thousands) \tab \bold{Numeric}\cr
#'   [, 2] \tab mode \tab Failure mode observed at \code{kilocycles} \tab \bold{Categoric}\cr
#'   [, 3] \tab event \tab Event observed at \code{kilocycles} (failure/right-censored/left-censored/interval-censored)  \tab \bold{Categoric}\cr
#'   [, 4] \tab count \tab Number of events observed at \code{kilocycles} \tab \bold{Numeric}\cr
#'   [, 5] \tab kilonewtons \tab Applied stress (in thousands) \tab \bold{Numeric}
#'    }
#' @source McCool, J. I. (1980), 
#'         Confidence limits for Weibull regression with censored data, 
#'         IEEE Transactions on Reliability, \bold{R-29}, 145-150.
#' @description McCool gives the results of a rolling contact fatigue test of ceramic 
#'              ball bearings. Ten specimens were tested across four levels of stress 
#'              measured in kilo-Newtons (1 kN = 1000 N). 
NULL

#' #####
#' 
#' @docType data
#' @family data-notdone
#' @name chemicalprocess   
#' @format A \code{data.frame} with 100 rows and 1 variable:
#' \tabular{rlll}{
#'   [, 1] \tab percent \tab Chemical concentration observed \tab \bold{Numeric}
#'    }
#' @source ####
#' @description ####
NULL

#' #####
#' 
#' @docType data
#' @family data-notdone
#' @name cirpack4   
#' @format A \code{data.frame} with 38 rows and 4 variables:
#' \tabular{rlll}{
#'   [, 1] \tab lower  \tab Start of an inspection interval in days \tab \bold{Numeric}\cr
#'   [, 2] \tab upper  \tab End of an inspection interval in days \tab \bold{Numeric}\cr
#'   [, 3] \tab event \tab Event observed in the interval (failure/right-censored/left-censored/interval-censored)  \tab \bold{Categoric}\cr
#'   [, 4] \tab count  \tab Number of events observed in the interval \tab \bold{Numeric}
#'    }
#' @source ####
#' @description ####
NULL

#' Circuit pack vendor comparison 
#'
#' @docType data
#' @family data-notdone
#' @name cirpack5
#' @format A \code{data.frame} with 41 rows and 4 variables:
#' \tabular{rlll}{
#'   [, 1] \tab days   \tab Accumulated time at \code{event} \tab \bold{Numeric}\cr
#'   [, 2] \tab event \tab Event observed at \code{days} (failure/right-censored/left-censored/interval-censored)  \tab \bold{Categoric}\cr
#'   [, 3] \tab count \tab Number of events observed at \code{days} \tab \bold{Numeric} \cr
#'   [, 4] \tab vendor \tab Producing vendor \tab \bold{Categoric}\cr
#'    }
#' @source Hooper, J.H. and Amster, S.J. (1990) Analysis and presentation of reliability data in Handbook of Statistical Methods for Engineers and Scientists, Harrison M. Wadsworth, editor. New York: McGraw Hill 
#' @description ####
NULL

#' ##### 
#' 
#' @docType data
#' @family data-notdone
#' @name componentd   
#' @format A \code{data.frame} with 24 rows and 3 variables:
#' \tabular{rlll}{
#'   [, 1] \tab months \tab Accumulated time at \code{event} \tab \bold{Numeric}\cr
#'   [, 2] \tab event \tab Event observed at \code{months} (failure/right-censored/left-censored/interval-censored)  \tab \bold{Categoric}\cr
#'   [, 3] \tab count \tab Number of events observed at \code{months} \tab \bold{Numeric}
#'    }
#' @source ####
#' @description ####
NULL

#' Computer program execution time data 
#' 
#' @docType data
#' @name comptime   
#' @format A \code{data.frame} with 17 rows and 3 variables:
#' \tabular{rlll}{
#'   [, 1] \tab users \tab Number of users \tab \bold{Numeric}\cr
#'   [, 2] \tab load \tab System load \tab \bold{Numeric}\cr
#'   [, 3] \tab seconds \tab Execution time observed at \code{load} \tab \bold{Numeric}
#'    }
#' @source Meeker W.Q. and Escobar L.A. (1998) Statistical Methods for Reliability Data, New York, NY; John Wiley & Sons.
#' @description Meeker and Escobar report the amount of time required for a Unix computer
#'              to execute a particular computer program on a multiuser computer system.
#'              The response (execution) times are a function of the total
#'              system load, which was obtained using the Unix \code{uptime}
#'              command. 
NULL

#' Computer lab reliability data 
#' 
#' @docType data
#' @name computerlab   
#' @format A \code{data.frame} with 101 rows and 3 variables:
#' \tabular{rlll}{
#'   [, 1] \tab computer \tab Designation code for the computer \tab \bold{Categoric}\cr
#'   [, 2] \tab days     \tab Accumulated time at \code{event} \tab \bold{Numeric}\cr
#'   [, 3] \tab event    \tab Event observed at \code{days} (Repair/End) \tab \bold{Categoric}
#'    }
#' @source Meeker W.Q. and Escobar L.A. (1998) Statistical Methods for Reliability Data, New York, NY; John Wiley & Sons.
#' @description A small un-monitored computer laboratory contains 10 networked
#'              microcomputers. Users who notice a hardware or software problem with
#'              a computer are supposed to report the problem to a technician who
#'              will fix the problem.  This data set includes the days in which trouble 
#'              calls were received, for each computer. Most of the trouble
#'              reports were easy to address (replace a defective mouse, reboot
#'              the computer, remake the computer's file system from the server, remove
#'              stuck floppy disk, tighten loose connector, etc.). All of the computers 
#'              were in operation for the entire semester (day 1 through 105).
NULL

#' Concrete fatigue-life test
#'
#' @docType data
#' @family data-notdone
#' @name concrete
#' @format A \code{data.frame} with 75 rows and 2 variables:
#' \tabular{rlll}{
#'   [, 1] \tab ratio \tab Stress ratio \code{max(tensile stress)/max(compressive stress)} \tab \bold{Numeric}\cr
#'   [, 2] \tab kilocycles \tab Accumulated cycles (in thousands) \tab \bold{Numeric}
#'    }
#' @source Castillo, E. and Hadi, Ali S., (1995) 
#'         Modeling lifetime data with application to fatigue models, 
#'         Journal of the American Statistical Association, \bold{90}, 1041-1054
#' @description ####
NULL

#' Circuit pack field tracking study 
#' 
#' @docType data
#' @name cirpack6   
#' @format A \code{data.frame} with 27 rows and 7 variables:
#' \tabular{rlll}{
#'   [, 1] \tab lower    \tab Start of an observation interval \tab \bold{Numeric}\cr
#'   [, 2] \tab upper    \tab End of an observation interval  \tab \bold{Numeric}\cr
#'   [, 3] \tab event    \tab Event observed in the interval (failure/right-censored/left-censored/interval-censored) \tab \bold{Categoric}\cr
#'   [, 4] \tab truntype \tab Truncation type (left-truncated/right-truncated/interval-truncated) \tab \bold{Categoric}\cr
#'   [, 5] \tab truntime \tab Truncation time \tab \bold{Numeric}\cr
#'   [, 6] \tab count    \tab Number of events observed in the interval \tab \bold{Numeric}\cr
#'   [, 7] \tab vendor   \tab Producing vendor \tab \bold{Categoric}
#'   }
#' @source Meeker, W. Q. and Escobar, L. A. (1998), Statistical Methods for Reliability Data, New York, NY; Wiley-Interscience
#' @description Circuit packs were manufactured to the same design 
#'              specification, but by two different vendors. The 
#'              trial ran for 10,000 hours to determine which vendor's
#'              circuit packs were more reliable.  The 4993 circuit packs 
#'              from Vendor 1 came straight from production.  
#'              The 4993 circuit packs from Vendor 2 had already seen 1000 
#'              hours of burn-in testing at the manufacturing plant under 
#'              operating conditions similar to those in the field trial. 
#'              The circuit packs manufactured by Vendor 2 were sold at a 
#'              higher price because field reliability was supposed to have 
#'              been improved by the burn-in screening of circuit packs 
#'              containing defective components. Failures during the first 
#'              1000 hours of burn-in were not recorded. This is the reason 
#'              for the unknown entries in the table and for having information 
#'              out to 11,000 hours for Vendor 2.  The data are for the first 
#'              failure in a position. Information on circuit packs replaced 
#'              after initial failure in a position was not part of the study.
NULL

#' ##### 
#' 
#' @docType data
#' @family data-notdone
#' @name connectionstrength   
#' @format A \code{data.frame} with 14 rows and 3 variables:
#' \tabular{rlll}{
#'   [, 1] \tab strength \tab Strength (in ???) of the connection \tab \bold{Numeric}\cr
#'   [, 2] \tab mode \tab Failure mode observed at \code{strength} \tab \bold{Categoric}\cr
#'   [, 3] \tab count \tab Number of failures observed at \code{strength}\tab \bold{Numeric}
#'    }
#' @source Nelson W. (1984), Applied Life Data Analysis, New York: John Wiley & Sons, pg. 111.
#' @description ####
NULL

#' #####
#' 
#' @docType data
#' @family data-notdone
#' @name customerlife   
#' @format A \code{data.frame} with 117 rows and 2 variables:
#' \tabular{rlll}{
#'   [, 1] \tab days \tab Accumulated time at \code{event} \tab \bold{Numeric}\cr
#'   [, 2] \tab event \tab Event observed at \code{days} (failure/right-censored/left-censored/interval-censored)  \tab \bold{Categoric}
#'    }
#' @source ####
#' @description ####
NULL

#' Diesel engine cylinder replacements 
#' 
#' @docType data
#' @name cylinder   
#' @format A \code{data.frame} with 276 rows and 4 variables:
#' \tabular{rlll}{
#'   [, 1] \tab engine \tab Type of engine used \tab \bold{Categoric}\cr
#'   [, 2] \tab days   \tab Accumulated time at \code{event} \tab \bold{Numeric}\cr
#'   [, 3] \tab count  \tab Number of events observed at \code{days} \tab \bold{Numeric}\cr
#'   [, 4] \tab event  \tab Event observed at \code{days} (Replacement/End) \tab \bold{Categoric}
#'   }
#' @source Nelson and Doganaksoy (1989),
#'         A computer program for an estimate and confidence limits for the mean cumulative function for cost or number of repairs of repairable products,
#'         TIS report 89CRD239, General Electric Company Research and Development, Schenectady, NY 
#' @description Nelson and Doganaksoy present data on cylinder replacement times for 120 diesel engines.  
#'              We take these engines to be a sample from a larger population of engines. Each engine has 16 cylinders. 
#' @details Cylinders in a type of diesel engine can develop leaks or have low compression. 
#'          Cylinders are inspected at times of convenience, along with other usual engine maintenance operations. 
#'          Faulty cylinders are replaced by a rebuilt cylinder.  More than one cylinder could be replaced at an inspection.  
#'          Management needed to know if the company should perform preventive replacement of cylinders before they develop 
#'          low compression failures.
NULL

#' Temperature-accelerated life test data 
#'
#' @docType data
#' @name devicea   
#' @format A \code{data.frame} with 37 rows and 4 variables:
#' \tabular{rlll}{
#'   [, 1] \tab hours   \tab Accumulated time at \code{event} \tab \bold{Numeric}\cr
#'   [, 2] \tab event   \tab Event observed at \code{hours} (failure/right-censored) \tab \bold{Categoric}\cr
#'   [, 3] \tab count   \tab Number of events observed at \code{hours} \tab \bold{Numeric}\cr
#'   [, 4] \tab celsius \tab Temperature applied to the device during testing \tab \bold{Numeric}
#'    }
#' @source Hoopers J. H. and Amster, S. J. (1990) 
#'         Analysis and presentation of reliability data, in Handbook of Statistical Methods for Engineers and Scientists, 
#'         McGraw-Hill, New York. Harrison M. Wadsworth, Editor.
#' @source Meeker W.Q. and Escobar L.A. (1998) Statistical Methods for Reliability Data, New York: John Wiley & Sons.         
#' @description Hooper and Amster (1990) analyze the temperature-accelerated life test data on an unidentified device. Meeker
#'              and Escobar (1998) refer to this device as "Device A". The purpose of the experiment was to determine if "Device A" 
#'              would meet a failure rate objective through 10,000 hours and 30,000 hours at an operating ambient temperature of 
#'              10 degrees celsius.  Device samples were tested for up to 5000 hours at four separate temperatures.
NULL

#' Power output degradation in integrated circuit devices
#'
#' @docType data
#' @name deviceb   
#' @format A \code{data.frame} with 570 rows and 4 variables:
#' \tabular{rlll}{
#'   [, 1] \tab powerdrop \tab Degradation (measured in decibels dB) of a device under test \tab \bold{Numeric}\cr
#'   [, 2] \tab device    \tab Label for the device under test \tab \bold{Categoric}\cr
#'   [, 3] \tab hours     \tab Time at which the degradation measure was observed \tab \bold{Numeric}\cr
#'   [, 4] \tab celsius   \tab Temperature applied \tab \bold{Numeric}
#'    }
#' @source Meeker W.Q. and Escobar L.A. (1998) Statistical Methods for Reliability Data, New York: John Wiley & Sons.
#' @description Samples of integrated circuit devices called "Device B" were tested at each of three levels of junction temperature. 
#'              The purpose of the test was to provide design engineers with an assessment of the proportion of these devices that 
#'              would "fail" before 15 years (about 130 thousand hours) of operation at 80 degrees celsius. Failure for an individual 
#'              device was defined as when the measured power output dropped more than 0.5 decibels (dB) below initial output. At 
#'              standard the operating temperature (80 degrees celsius), the devices will degrade too slowly to provide useful 
#'              information in 6 months.  Because units at low temperature degrade more slowly, they had to be run for longer periods 
#'              of time to accumulate appreciable degradation.  Because of severe limitations in the number of test positions, fewer 
#'              units were run at lower temperatures. 
#'              
#'              Note: The original data from this experiment are proprietary. The observations in this dataset were actually simulated 
#'              from a model suggested by physical theory and limited real data that was available at the time when the more complete 
#'              experiment was being planned. 
NULL

#' Integrated circuit accelerated life test data
#'
#' @docType data
#' @name devicec   
#' @format A \code{data.frame} with 26 rows and 4 variables:
#' \tabular{rlll}{
#'   [, 1] \tab celsius   \tab Temperature to which a device was exposed during the test \tab \bold{Numeric}\cr
#'   [, 2] \tab kilohours \tab Accumulated time at \code{event} \tab \bold{Numeric}\cr
#'   [, 3] \tab event     \tab Event observed at \code{hours} (failure/right-censored)  \tab \bold{Categoric}\cr
#'   [, 4] \tab count     \tab Number of events observed at \code{hours} \tab \bold{Numeric}
#'    }
#' @source Meeker W.Q. and Escobar L.A. (1998) Statistical Methods for Reliability Data, New York, NY; John Wiley & Sons.
#' @description Observations from an accelerated life test performed on an integrated circuit device called "Device C". Failures 
#'              were caused by a chemical reaction inside the circuit package. Reliability engineers tested 10 circuits at five
#'              different temperatures over a period of 3000 hours. The purpose of the experiment was to estimate the activation 
#'              energy of the failure-causing reaction and to obtain an estimate of the integrated circuit life distribution at 
#'              an 80 degrees celsius junction temperature.
NULL

#' ##### 
#'
#' @docType data
#' @family data-notdone
#' @rdname deviced
#' @name devicedconnected   
#' @name devicednotconnected
#' @format A \code{data.frame} with 986 rows and 9 variables:
#' \tabular{rlll}{
#'   [, 1] \tab weeks \tab Failure time \tab \bold{Numeric}\cr
#'   [, 2] \tab cycles \tab Total accumulated cycles at failure \tab \bold{Numeric}\cr
#'   [, 3] \tab causeoffailure.fm1 \tab Type of event observed (failure, right-censored, left-censored, or interval-censored)  \tab \bold{Categoric}\cr
#'   [, 4] \tab causeoffailure.fm2 \tab Start time (in hours) of an inspection interval \tab \bold{Numeric}\cr
#'   [, 5] \tab causeoffailure.fm3 \tab Time (in hours) at which an interval ends \tab \bold{Numeric}\cr
#'   [, 6] \tab causeoffailure.fmother \tab Type of event observed (failure, right-censored, left-censored, or interval-censored)  \tab \bold{Categoric}\cr
#'   [, 7] \tab perweek \tab Average number of cycles (across all users) \tab \bold{Numeric}\cr
#'   [, 8] \tab weeksinserted \tab Time inserted into service \tab \bold{Numeric}\cr
#'   [, 9] \tab weeksreturned \tab Time return to manufacturer \tab \bold{Numeric}
#'    }
#' @source ####
#' @description ####
NULL

#' Field tracking study with multiple failure modes
#'
#' @docType data
#' @name deviceg
#' @format A \code{data.frame} with 30 rows and 2 variables:
#' \tabular{rlll}{
#'   [, 1] \tab kilocycles \tab Accumulated cycles at failure (in thousands) \tab \bold{Numeric}\cr
#'   [, 2] \tab mode \tab Failure mode observed at \code{kcycles} (Wearout/Surge/Suspension) \tab \bold{Categoric}
#'    }
#' @source Meeker W.Q. and Escobar L.A. (1998) Statistical Methods for Reliability Data, New York, NY; John Wiley & Sons.
#' @description Thirty (30) samples of a device that was part of a power generation system were tested as part of a field tracking study 
#'              for up to 300 kilocycles. The devices under test were installed in typical service environments and at the occurence 
#'              of a failure, the observed number of cycles and the type of failure were recorded. 
#'              Surge failures, which predominated early in the device life-cycle, were caused by an accumulation of randomly occurring 
#'              damage from power-line voltage spikes during electric storms resulting in failure of a particular unprotected electronic 
#'              component. Wearout failures resulted from normal product wear and began to appear after 100 thousand cycles of use.
NULL

#' #####
#'
#' @docType data
#' @family data-notdone
#' @name deviceh
#' @format A \code{data.frame} with 38 rows and 3 variables:
#' \tabular{rlll}{
#'   [, 1] \tab kilocycles \tab Accumulated cycles at \code{event} (in thousands) \tab \bold{Numeric}\cr
#'   [, 2] \tab event \tab Event observed at \code{kcycles} (failed/survived)  \tab \bold{Categoric}\cr
#'   [, 3] \tab count \tab Number of events observed at \code{kcycles} \tab \bold{Numeric}
#'    }
#' @source Doganaksoy N., Hahn, G. J., and Meeker, W. Q. (2000), Product life analysis: a case study, Quality Progress, June 2000.
#' @description ####
NULL

#' ##### 
#'
#' @docType data
#' @family data-notdone
#' @name devicen   
#' @format A \code{data.frame} with 24 rows and 3 variables:
#' \tabular{rlll}{
#'   [, 1] \tab months \tab Accumulated time at \code{event} \tab \bold{Numeric}\cr
#'   [, 3] \tab event \tab Event observed at \code{months} (right-censored/left-censored)  \tab \bold{Categoric}\cr
#'   [, 4] \tab counts \tab Number of events observed at \code{months} \tab \bold{Numeric}
#'    }
#' @source ####
#' @description ####
NULL

#' ##### 
#'
#' @docType data
#' @family data-notdone
#' @name diskber   
#' @format A \code{data.frame} with 80 rows and 4 variables:
#' \tabular{rlll}{
#'   [, 1] \tab rate \tab Observed error rate of \code{disk}  \tab \bold{Numeric}\cr
#'   [, 2] \tab disk \tab Disk type (1 - 16) \tab \bold{Categoric}\cr
#'   [, 3] \tab hours \tab Event time \tab \bold{Numeric}\cr
#'   [, 4] \tab celsius \tab Temperature applied to \code{disk} \tab \bold{Numeric}
#'    }
#' @source Murray, W. P. (1993), 
#'         Archival life expectancy of 3M-magneto-optic media, 
#'         Journal of the Magnetics Society of Japan \bold{17}, Supplement S1, 309-314.
#' @description ####
NULL

#' #####
#'
#' @docType data
#' @family data-notdone
#' @name doatrun
#' @format A \code{data.frame} with 48 rows and 6 variables:
#' \tabular{rlll}{
#'   [, 1] \tab lower \tab Start of an inspection interval \tab \bold{Numeric}\cr
#'   [, 2] \tab upper \tab End of an inspection interval \tab \bold{Numeric}\cr
#'   [, 3] \tab censor \tab Event observed in the interval (right-censored/interval-censored)  \tab \bold{Categoric}\cr
#'   [, 4] \tab count \tab Number of events observed in the interval \tab \bold{Numeric} \cr
#'   [, 5] \tab truntime \tab Truncation time \tab \bold{Numeric}\cr
#'   [, 6] \tab truntype \tab Truncation type  \tab \bold{Categoric}
#'    }
#' @source ####
#' @description data	truncated	because	of	burn-in	and	removal	of	an	unknown	number of	DOAs
NULL

#' ##### 
#' 
#' @docType data
#' @family data-notdone
#' @name electromech   
#' @format A \code{data.frame} with 9 rows and 4 variables:
#' \tabular{rlll}{
#'   [, 1] \tab lower \tab Start of an inspection interval (in months) \tab \bold{Numeric}\cr
#'   [, 2] \tab upper \tab End of an inspection interval (in months) \tab \bold{Numeric}\cr
#'   [, 3] \tab event \tab Event observed in the interval (right-censored/left-censored/interval-censored)  \tab \bold{Categoric}\cr
#'   [, 4] \tab count \tab Number of events observed in the interval \tab \bold{Numeric}
#'    }
#' @source Hahn and Meeker (1982a)
#' @description ####
NULL

#' ##### 
#' 
#' @docType data
#' @family data-notdone
#' @name engineemissions   
#' @format A \code{data.frame} with 16 rows and 1 variable:
#' \tabular{rlll}{
#'   [, 1] \tab emissions \tab ##### \tab \bold{Numeric}
#'    }
#' @source Meeker W.Q., Escobar L.A., and Lu (1998)
#' @description ####
NULL

#' NIST epoxy weathering test 
#' 
#' @docType data
#' @family data-notdone
#' @name epoxyweathering   
#' @format A \code{data.frame} with 209 rows and 3 variables:
#' \tabular{rlll}{
#'   [, 1] \tab spec \tab Type of epoxy tested \tab \bold{Categoric}\cr
#'   [, 2] \tab dosage \tab ##### \tab \bold{Numeric}\cr
#'   [, 3] \tab damage \tab #####  \tab \bold{Numeric}
#'    }
#' @source Meeker W.Q. and Escobar L.A. (1998) Statistical Methods for Reliability Data, New York, NY; John Wiley & Sons.
#' @description ####
NULL

#' Diesel generator fan failures 
#' 
#' @docType data
#' @name fan  
#' @format A \code{data.frame} with 37 rows and 3 variables:
#' \tabular{rlll}{
#'   [, 1] \tab hours \tab Accumulated time at \code{event} \tab \bold{Numeric}\cr
#'   [, 2] \tab event \tab Event observed at \code{hours} (failure/right-censored) \tab \bold{Categoric}\cr
#'   [, 3] \tab count \tab Number of events observed at \code{hours} \tab \bold{Numeric}
#'   }
#' @source Nelson, W. (1982), Applied Life Data Analysis, pg 133, New York, John Wiley & Sons Inc.
#' @description  Failures in 12 of 70 generator fans were reported at times ranging between 450 hours and 8,750 hours. 
#'               Of the 58 units that did not fail, the reported running times (i.e., censoring times) ranged between 
#'               460 and 11,500 hours. Different fans had different running times because units were introduced into 
#'               service at different times and because their use-rates differed.
NULL

#' Wire filament test design data
#'
#' @docType data
#' @family data-notdone
#' @name filament    
#' @format A \code{data.frame} with 6 rows and 7 variables:
#' \tabular{rlll}{
#'   [, 1] \tab celsius \tab Temperature applied \tab \bold{Numeric}\cr
#'   [, 2] \tab volts \tab Electrical power applied \tab \bold{Numeric}\cr
#'   [, 3] \tab weeks1 \tab Number of units tested for one week \tab \bold{Categoric}\cr
#'   [, 4] \tab weeks2 \tab Number of units tested for two weeks \tab \bold{Categoric}\cr
#'   [, 5] \tab weeks5 \tab Number of units tested for five weeks \tab \bold{Categoric}\cr
#'   [, 6] \tab weeks10 \tab Number of units tested for ten weeks \tab \bold{Categoric}\cr
#'   [, 7] \tab weeks20 \tab Number of units tested for twenty week \tab \bold{Categoric}
#'    }
#' @source ####
#' @description ####
NULL

#' Gallium-Arsenic Laser Degradation Test 
#' 
#' @docType data
#' @name gaaslaser
#' @format A \code{data.frame} with 255 rows and 3 variables:
#' \tabular{rlll}{
#'   [, 1] \tab increase \tab Percent increase in observed operating current \tab \bold{Numeric}\cr
#'   [, 2] \tab unit \tab Unit designator code \tab \bold{Categoric}\cr
#'   [, 3] \tab hours \tab Accumulated time when \code{increase} was measured \tab \bold{Numeric}
#'    }
#' @source Meeker W.Q. and Escobar L.A. (1998) Statistical Methods for Reliability Data, New York, NY; John Wiley & Sons.
#' @description Over the life of some lasers devices, degradation causes
#'              a decrease in light output.  In this test, a feedback mechanism was used 
#'              to maintain a constant light output by increasing the operating current.
#'              Fifteen GaAs laser devices were tested at an elevated temperature of 
#'              80^{o} C to accelerate the degradation process.  The operating current
#'              was measured in 250-hour intervals and the data were recorded as the
#'              percent increase in current compared to the initial operating current
#'              measured when the test began.  A device was considered to have failed
#'              if the percent increase reached 10\%.
NULL

#' Unscheduled maintenance actions for the U.S.S. Grampus 
#'
#' @docType data
#' @name grampus   
#' @format A \code{data.frame} with 57 rows and 3 variables:
#' \tabular{rlll}{
#'   [, 1] \tab unit   \tab Unit type \tab \bold{Categoric}\cr
#'   [, 2] \tab kilohours \tab Accumulated hours at \code{event} (in thousands) \tab \bold{Numeric}\cr
#'   [, 3] \tab event \tab Event observed at \code{khours} (Repair/End)  \tab \bold{Categoric}
#'    }
#' @source Lee, L. (1980) 
#'         Testing adequacy of the Weibull and loglinear rate models for a Poisson Process,
#'         Technometrics, 22, 195-199  
#' @source Ascher, H. and Feingold, H. (1984), Repairable Systems Reliability, New York, NY; Marcel Dekker
#' @description Lee (1980) presents a dataset containing the times (in thousands of operating hours) of unscheduled maintenance actions on the 
#'              number 4 diesel engine of the U.S.S. Grampus. The data contain observations of maintenance actions for the first 16,000 hours of operation.  
#'              The observations in this dataset should be treated as is they were observed from a single system, since information as 
#'              to which component in the engine failed was not included. The unscheduled maintenance actions were caused by either system 
#'              failures or by events that indicated that a system failure was imminent. Such maintenance actions are inconvenient and 
#'              expensive.
NULL

#' Locomotive braking grid replacements 
#'
#' @docType data
#' @family data-done
#' @rdname grids
#' @name grids1
#' @name grids2
#' @format A \code{data.frame} with 39 rows and 3 variables:
#' \tabular{rlll}{
#'   [, 1] \tab unit \tab Unit designator code \tab \bold{Categoric}\cr
#'   [, 2] \tab days \tab Accumulated time at \code{event} \tab \bold{Numeric}\cr
#'   [, 3] \tab event \tab Event observed at \code{days} (Replacement/End)  \tab \bold{Categoric}
#'    }
#' @source Doganaksoy, N., and Nelson, W. (1991) A method and computer program MCFDIFF to compare two samples of repair data,
#'         TIS Report 91CRD172, General Electric Company Research and Development, Schenectady, NY.
#' @description Age of locomotives when either their braking grids were replaced or the largest age observed for each locomotive
#' @details Two batches \code{grids1} & \code{grids2}
NULL

#' Unscheduled maintenance actions for the U.S.S. Halfbeak 
#'
#' @docType data
#' @name halfbeak   
#' @format A \code{data.frame} with 73 rows and 3 variables:
#' \tabular{rlll}{
#'   [, 1] \tab unit \tab Unit designator code \tab \bold{Categoric}\cr
#'   [, 2] \tab hours \tab Accumulated time at \code{event} \tab \bold{Numeric}\cr
#'   [, 3] \tab event \tab Event observed at \code{hours} (Start/Report/End)  \tab \bold{Categoric}
#'    }
#' @source Ascher, H. and Feingold, H. (1984), Repairable Systems Reliability, New York, NY; Marcel Dekker
#' @description Ascher and Feingold (1984) present a dataset containing unscheduled maintenance actions for the U.S.S. Halfbeak number 4 main 
#'              propulsion diesel engine over 25,518 operating hours. For each observation, an event was recorded as either \code{start} 
#'              (denotes the start of the observational period), \code{end} (denotes the end of the observational period), or \code{report}
#'              (denoting that an unscheduled maintenance action was reported). The data were analyzed to determine if the system was 
#'              deteriorating (unscheduled maintenance actions were being reported more frequently as the system ages)
NULL

#' ##### 
#'
#' @docType data
#' @family data-notdone
#' @name hcfdata   
#' @format A \code{data.frame} with 83 rows and 9 variables:
#' \tabular{rlll}{
#'   [, 1] \tab specimen \tab Specimen type \tab \bold{Categoric}\cr
#'   [, 2] \tab step \tab Was step-up loading used? \tab \bold{Categoric}\cr
#'   [, 3] \tab hertz \tab Load frequency \tab \bold{Numeric}\cr
#'   [, 4] \tab ratio \tab Stress ratio \code{max(tensile stress)/max(compressive stress)} \tab \bold{Numeric}\cr
#'   [, 5] \tab cycles \tab Accumulated cycles at \code{event} \tab \bold{Numeric}\cr
#'   [, 6] \tab range \tab Stress range \code{max(tensile stress) - max(compressive stress)} \tab \bold{Numeric}\cr
#'   [, 7] \tab tensile \tab Maximum tensile stress \tab \bold{Numeric}\cr
#'   [, 8] \tab event \tab Event observed at \code{cycles} (failed/right-censored) \tab \bold{Categoric}\cr
#'   [, 9] \tab swt \tab Smith-Watson-Topper parameter \tab \bold{Numeric}
#'    }
#' @source W.Q. Meeker, L.A. Escobar, and Lu (1998)
#' @details \deqn{swt = 0.5 x \sigma\Delta\epsilon}
#' @description ####
NULL

#' Sheathed tubular heater failures 
#'
#' @docType data
#' @family data-notdone
#' @name heater   
#' @format A \code{data.frame} with 24 rows and 2 variables:
#' \tabular{rlll}{
#'   [, 1] \tab hours      \tab Accumulated time at failure \tab \bold{Numeric}\cr
#'   [, 2] \tab fahrenheit \tab Temperature applied \tab \bold{Numeric}
#'    }
#' @source Nelson (1990)
#' @description ####
NULL

#' Nuclear power plant heat exchanger tube crack data 
#' 
#' @docType data
#' @name heatexchanger
#' @format A \code{data.frame} with 9 rows and 5 variables:
#' \tabular{rlll}{
#'   [, 1] \tab lower \tab Start of an inspection interval (in years) \tab \bold{Numeric}\cr
#'   [, 2] \tab upper \tab End of an inspection interval (in years) \tab \bold{Numeric}\cr
#'   [, 3] \tab event \tab Event observed in the interval (left-censored/right-censored/interval-censored)\tab \bold{Categoric}\cr
#'   [, 4] \tab count \tab Number of events observed in the interval \tab \bold{Numeric}\cr
#'   [, 5] \tab plant  \tab Plant where the tube was installed \tab \bold{Categoric}
#'  }   
#' @source Meeker, W. Q. and Escobar, L. A. (1996), Statistical Methods for Reliability Data, New York, NY; Wiley-Interscience
#' @description Inspection data for heat exchanger tubes across three different nuclear power plants.  The data were recorded in 1983,
#'              at this point in time, Plant 1 had been in operation for 3 years, Plant 2 for 2 years, and Plant 3 for only 1 year. 
#'              Because all of the heat exchangers were manufactured according to the same design specifications and because the heat 
#'              exchangers were operated in generating plants run under similar tightly controlled conditions, the data from the 
#'              different plants was combined for the sake of making inferences and predictions about the time-to-crack distribution 
#'              of the heat exchanger tubes.
#' @details Nuclear power plants use heat exchangers to transfer energy from the reactor to steam turbines.  A typical heat exchanger 
#'          contains thousands of tubes through which steam flows continuously when the heat exchanger is in service.  With age, 
#'          heat exchanger tubes develop cracks, usually due to some combination of stress-corrosion and fatigue. A heat exchanger 
#'          can continue to operate safely when the cracks are small.  If cracks get large enough, however, leaks can develop, and 
#'          these could lead to serious safety problems and expensive, unplanned plant shut-down time. To protect against having leaks, 
#'          heat exchangers are taken out of service periodically so that its tubes (and other components) can be inspected with 
#'          nondestructive evaluation techniques.  At the end of each inspection period, tubes with detected cracks are plugged so that 
#'          water will no longer pass through them.  This reduces plant efficiency, but extends the life of the expensive heat exchangers. 
#'          With this in mind, heat exchangers are built with extra capacity and can remain in operation up until the point where a certain 
#'          percentage (e.g., 5\%) of the tubes have been plugged.
NULL

#' ##### 
#'
#' @docType data
#' @family data-notdone
#' @name hpcrepairs   
#' @format A \code{data.frame} with 8 rows and 3 variables:
#' \tabular{rlll}{
#'   [, 1] \tab system \tab System type \tab \bold{Categoric}\cr
#'   [, 2] \tab months \tab Accumulated time at \code{event} \tab \bold{Numeric}\cr
#'   [, 3] \tab event \tab Event observed at \code{months} (Repair/End) \tab \bold{Categoric}
#'    }
#' @source Shimokawa, T., and Hamaguchi, Y. (1987), Statistical Evaluation of Fatigue Life and Fatigue Strength in Circular-Holed Notched Specimens of a Carbon Eight-Harness-Satin/Epoxy Laminate, in Statistical Research on Fatigue and Fracture (Current Japanese Materials Research, Vol. 2), eds. T. Tanaka, S. Nishijima, and M. Ichikawa, London: Elsevier, pp. 159-176.
#' @description ####
NULL

#' ##### 
#'
#' @docType data
#' @family data-notdone
#' @name icdevice1
#' @name icdevice2
#' @name icdevice2_w300
#' @rdname icdevice   
#' @format A \code{data.frame} with 8 rows and 4 variables:
#' \tabular{rlll}{
#'   [, 1] \tab lower \tab Start of an inspection interval (in hours) \tab \bold{Numeric}\cr
#'   [, 2] \tab upper \tab End of an inspection interval (in hours) \tab \bold{Numeric}\cr
#'   [, 3] \tab event \tab Event observed in the interval (right-censored/interval-censored)  \tab \bold{Categoric}\cr
#'   [, 4] \tab count \tab Number of events observed in the interval \tab \bold{Numeric}\cr
#'   [, 4] \tab celsius \tab Temperature applied \tab \bold{Numeric}
#'    }
#' @source Meeker and Escobar (1998)
#' @description ####
NULL

#' ##### 
#'
#' @docType data
#' @family data-notdone
#' @name insulation
#' @name insulation_dadtplan
#' @rdname insulation   
#' @format A \code{data.frame} with 128 rows and 4 variables:
#' \tabular{rlll}{
#'   [, 1] \tab weeks \tab Event time \tab \bold{Numeric}\cr
#'   [, 2] \tab celsius \tab Temperature applied \tab \bold{Numeric}\cr
#'   [, 3] \tab volts \tab Electrical power applied  \tab \bold{Numeric}\cr
#'   [, 4] \tab units \tab Number of events observed at \code{weeks} \tab \bold{Numeric}
#'    }
#' @source ####
#' @description ####
NULL

#' #####
#' 
#' @docType data
#' @family data-notdone
#' @name inconel   
#' @format A \code{data.frame} with 246 rows and 3 variables:
#' \tabular{rlll}{
#'   [, 1] \tab strain \tab Amount of strain observed \tab \bold{Numeric}\cr
#'   [, 2] \tab cycles \tab Accumulated cycles at \code{event} \tab \bold{Numeric}\cr
#'   [, 3] \tab event \tab Event observed at \code{cycles} (Failed/right-censored)  \tab \bold{Categoric}
#'    }
#' @source Shen, C. L. (1994), Statistical Analysis of Fatigue Data,unpublished Ph.D. dissertation, University of Arizona, Department of Aerospace and Mechanical Engineering.
#' @description ####
NULL

#' ##### 
#'
#' @docType data
#' @family data-notdone
#' @name laminatepanel
#' @format A \code{data.frame} with 125 rows and 3 variables:
#' \tabular{rlll}{
#'   [, 1] \tab mpa \tab Stress applied (in millions of pascals) \tab \bold{Numeric}\cr
#'   [, 2] \tab kilocycles \tab Accumulated cycles at \code{event} (in thousands) \tab \bold{Numeric}\cr
#'   [, 3] \tab event \tab Event observed at \code{kilocycles} (failed/right-censored)  \tab \bold{Categoric}\
#'    }
#' @source Shimokawa, T., and Hamaguchi, Y. (1987), Statistical Evaluation of Fatigue Life and Fatigue Strength in Circular-Holed Notched Specimens of a Carbon Eight-Harness-Satin/Epoxy Laminate,'' in Statistical Research on Fatigue and Fracture (Current Japanese Materials Research, Vol. 2), eds. T. Tanaka, S. Nishijima, and M. Ichikawa, London: Elsevier, pp. 159-176.
#' @description ####
NULL

#' ##### 
#'
#' @docType data
#' @family data-notdone
#' @name largeball
#' @format A \code{data.frame} with 13 rows and 5 variables:
#' \tabular{rlll}{
#'   [, 1] \tab lower \tab Start of an inspection interval (in hours) \tab \bold{Numeric}\cr
#'   [, 2] \tab upper \tab End of an inspection interval (in hours) \tab \bold{Numeric}\cr
#'   [, 3] \tab event \tab Event observed in the interval (interval-censored)  \tab \bold{Categoric}\cr
#'   [, 4] \tab count \tab Number of events observed in the interval\tab \bold{Numeric}\cr
#'   [, 5] \tab celsius \tab Temperature applied \tab \bold{Numeric}
#'    }
#' @source Meeker W.Q., Escobar L.A., and Lu (1998)
#' @description ####
NULL

#' Integrated circuit life test 
#'
#' @docType data
#' @name lfp1370  
#' @format A \code{data.frame} with 22 rows and 3 variables:
#' \tabular{rlll}{
#'   [, 1] \tab hours \tab Accumulated time at \code{event} \tab \bold{Numeric}\cr
#'   [, 2] \tab event \tab Event observed at \code{hours} (failure/right-censored) \tab \bold{Categoric}\cr
#'   [, 3] \tab count \tab Number of events observed at \code{hours} \tab \bold{Numeric}
#'   }
#' @source Meeker, W. Q. (1987), Limited failure population life tests: application to integrated circuit reliability, Technometrics, 29, 51-65.
#' @description The primary purpose of this experiment was to estimate the proportion of 
#'              defective units being manufactured in the current production process and 
#'              to estimate the amount of 'burn-in' time that would be required to remove 
#'              most of the defective units from the product population.  The engineers 
#'              involved in the experiment were also interested in whether it might be 
#'              possible to get the needed information about the state of the production process. 
#'              In the future, using much shorter tests (say 200 or 300 hours).
#'              
#' @details The \code{event} column indicates that these data are singly right censored at 1370 hours.
#'          However, the presence of ties indicates that the data are actually inspection times which 
#'          perhaps should have been recorded as interval censored observations.
#' @seealso \code{\link{lfptrun100}}
NULL

#' Integrated circuit life test (truncated) 
#'
#' @docType data
#' @name lfptrun100
#' @format A \code{data.frame} with 28 rows and 34variables:
#' \tabular{rlll}{
#'   [, 1] \tab hours \tab Accumulated time at \code{event} \tab \bold{Numeric}\cr
#'   [, 2] \tab count \tab Event observed at \code{hours} (failure/right-censored) \tab \bold{Categoric}\cr
#'   [, 3] \tab truntime \tab Truncation time \tab \bold{Numeric}\cr
#'   [, 4] \tab truntype \tab Truncation type \code{hours} \tab \bold{Categoric}
#'   }
#' @source Meeker, W. Q. (1987), Limited failure population life tests: application to integrated circuit reliability, Technometrics, 29, 51-65.
#' @description The primary purpose of this experiment was to estimate the proportion of 
#'              defective units being manufactured in the current production process and 
#'              to estimate the amount of 'burn-in' time that would be required to remove 
#'              most of the defective units from the product population.  The engineers 
#'              involved in the experiment were also interested in whether it might be 
#'              possible to get the needed information about the state of the production process. 
#'              In the future, using much shorter tests (say 200 or 300 hours).
#'              
#' @details The \code{event} column indicates that these data are singly right censored at 1370 hours.
#'          However, the presence of ties indicates that the data are actually inspection times which 
#'          perhaps should have been recorded as interval censored observations.
#' @seealso \code{\link{lfp1370}}
NULL

#' ##### 
#' 
#' @docType data
#' @family data-notdone
#' @name locomotivecontrol   
#' @format A \code{data.frame} with 38 rows and 3 variables:
#' \tabular{rlll}{
#'   [, 1] \tab kilomiles \tab Accumulated distance at \code{event} (in thousands) \tab \bold{Numeric}\cr
#'   [, 2] \tab event \tab Event observed at \code{kilomiles} (failed/right-censored)  \tab \bold{Categoric}\cr
#'   [, 3] \tab count \tab Number of events observed at \code{kilomiles} \tab \bold{Numeric}
#'    }
#' @source Nelson (1982), page 33
#' @description ####
NULL

#' #####
#'
#' @docType data
#' @family data-notdone
#' @name luminosity
#' @format A \code{data.frame} with 2175 rows and 4 variables:
#' \tabular{rlll}{
#'   [, 1] \tab hours \tab Accumulated time when \code{luminosity} was measured \tab \bold{Numeric}\cr
#'   [, 2] \tab celsius \tab Temperature applied \tab \bold{Numeric}\cr
#'   [, 3] \tab unit \tab Unit type \tab \bold{Categoric}\cr
#'   [, 4] \tab luminosity \tab Luminosity of something \tab \bold{Numeric}
#'    }
#' @source Meeker W.Q. and Escobar L.A. (1998) Statistical Methods for Reliability Data, New York, NY; John Wiley & Sons.
#' @description ####
NULL

#' Ball bearing fatigue test data
#' 
#' @docType data
#' @name lzbearing  
#' @format A \code{data.frame} with 23 rows and 1 variable:
#' \tabular{rlll}{
#'   [, 1] \tab megacycles \tab Accumulated cycles at failure (in millions) \tab \bold{Numeric}
#'  }
#' @source Lawless, J. F. (1982), Statistical Models and Methods for Lifetime Data, New York, NY; Wiley & Sons
#' @description The ball bearings came from four different major bearing companies. 
#'              There was disagreement in the industry as to the appropriate parameter 
#'              values to use to describe the relationship between fatigue life and stress 
#'              loading. The main objective of the study was to estimate values of the parameters 
#'              in the equation relating bearing life to load.
NULL

#' Earth-moving machine maintenance  
#'
#' @docType data
#' @family data-notdone
#' @name machineh   
#' @format A \code{data.frame} with 573 rows and 4 variables:
#' \tabular{rlll}{
#'   [, 1] \tab unit \tab Unit type \tab \bold{Categoric}\cr
#'   [, 2] \tab hours \tab Accumulated time at \code{event} \tab \bold{Numeric}\cr
#'   [, 3] \tab cost \tab Price of test \tab \bold{Numeric}\cr
#'   [, 4] \tab event \tab Event observed at \code{hours} (Action/End)  \tab \bold{Categoric}
#'  }
#' @source Meeker W.Q. and Escobar L.A. (1998) Statistical Methods for Reliability Data, New York, NY; John Wiley & Sons.
#' @description ####
NULL

#' ##### 
#' 
#' @docType data
#' @family data-notdone
#' @name mechanicalswitch
#' @format A \code{data.frame} with 37 rows and 2 variables:
#' \tabular{rlll}{
#'   [, 1] \tab hours\tab Accumulated time at failure \tab \bold{Numeric}\cr
#'   [, 2] \tab mode \tab Failure mode observed at \code{hours} \tab \bold{Categoric}
#'    }
#' @source Nair (1994)
#' @description ####
NULL

#' Metal alloy sliding wear resistance test 
#' 
#' @docType data
#' @name metalwear
#' @format A \code{data.frame} with 96 rows and 4 variables:
#' \tabular{rlll}{
#'   [, 1] \tab microns \tab Specimen thickness \tab \bold{Numeric}\cr
#'   [, 2] \tab unit \tab Unit designator \tab \bold{Categoric}\cr
#'   [, 3] \tab cycles \tab Accumulated cycles at an observation \tab \bold{Numeric}\cr
#'   [, 4] \tab grams \tab Weight applied to the test specimen \tab \bold{Numeric}
#'    }
#' @source Meeker, W. Q. and Escobar, L. A. (1998) Statistical Methods for Reliability Data, New York, NY; John Wiley & Sons.
#' @description An experiment was conducted to test the sliding wear resistance of a particular metal alloy. 
#'              The data show changes in specimen thickness after being subjected to metal-to-metal sliding friction.
#'              Adding weight can increase the sliding frictional forces and cause the specimen to wear more quickly.
#'              A range of different weights were applied to (1) study the relationship between applied weight and wear 
#'              resistance and (2) gain a better understanding of the wear mechanism.
NULL

#' ##### 
#' 
#' @docType data
#' @family data-notdone
#' @name multiplefail   
#' @format A \code{data.frame} with 40 rows and 7 variables:
#' \tabular{rlll}{
#'   [, 1] \tab celsius \tab Temperature applied \tab \bold{Numeric}\cr
#'   [, 2] \tab turn_hours \tab Time of an observed turn event \tab \bold{Numeric}\cr
#'   [, 3] \tab turn_event \tab Type of turn event at \code{turn_hours} (failed/right-censored) \tab \bold{Categoric}\cr
#'   [, 4] \tab phase_hours \tab Time of an observed phase event \tab \bold{Numeric}\cr
#'   [, 5] \tab phase_event \tab Type of phase event at \code{phase_hours} (failed/right-censored) \tab \bold{Categoric}\cr
#'   [, 6] \tab ground_hours \tab Time of an observed ground event \tab \bold{Numeric}\cr
#'   [, 7] \tab ground_event \tab Type of ground event at \code{ground_hours} (failed/right-censored) \tab \bold{Categoric}
#'    }
#' @source Nelson (1990)
#' @description ####
NULL

#' Accelerated life test of a mylar-polyurethane insulating structure 
#' 
#' @rdname mylarpoly
#' @docType data
#' @name mylarsub
#' @name mylarpoly   
#' @format A \code{data.frame} with 46 rows and 2 variables:
#' \tabular{rlll}{
#'   [, 1] \tab minutes \tab Time to dielectric breakdown \tab \bold{Numeric}\cr
#'   [, 2] \tab ratio   \tab Electromagnetic field strength (in kV/mm) \tab \bold{Numeric}
#'    }
#' @source Kalkanis, G., and Rosso, E. (1989),
#'         The inverse power law model for the lifetime of a mylar-polyurethane laminated DC HV insulating structure, 
#'         Nuclear Instruments and Methods in Physics Research, \bold{A281}, 489-496.
#' @description Kalkanis and Rosso (1989) present data generated from an accelerated life test performed on a 
#'              special type of mylar-polyurethane insulation used in high-performance electro-magnets.
#'              The data give the time to dielectric breakdown of units tested at 100.3, 122.4, 157.1, 
#'              219.0, and 361.4 kV/mm.  The purpose of the experiment was to evaluate the reliability 
#'              of the insulating structure and to estimate the life distribution at system design voltages.
NULL

#' High-temp gas spring accelerated test
#' 
#' @docType data
#' @family data-notdone
#' @name newspring   
#' @format A \code{data.frame} with 80 rows and 6 variables:
#' \tabular{rlll}{
#'   [, 1] \tab kilocycles \tab Accumulated cycles at \code{event} (in thousands) \tab \bold{Numeric}\cr
#'   [, 2] \tab centimeters \tab Spring stroke length \tab \bold{Numeric}\cr
#'   [, 3] \tab fahrenheit \tab Temperature applied to the spring \tab \bold{Numeric}\cr
#'   [, 4] \tab method \tab Manufacturing method (New/Old) \tab \bold{Categoric}\cr
#'   [, 5] \tab event \tab Event observed at \code{kcycles} (Suspended/Failed) \tab \bold{Categoric}\cr
#'   [, 6] \tab count \tab Number of events observed at \code{kcycles} \tab \bold{Numeric}
#'   }
#' @source Meeker, W. Q. (1999) A factorial experiment to compare the lifetimes of springs as a function of a processing temperature and amount of displacement in the spring test (Unpublished)
#' @description ####
NULL

#' Accelerated test of spacecraft battery cell data
#'
#' @docType data
#' @name nicdbattery
#' @format A \code{data.frame} with 87 rows and 10 variables:
#' \tabular{rlll}{
#'   [, 1] \tab celsius \tab Temperature applied to the battery \tab \bold{Numeric}\cr
#'   [, 2] \tab discharge_depth \tab Depth of discharge (as percent) \tab \bold{Numeric}\cr
#'   [, 3] \tab discharge_time \tab Discharge time (in hours) \tab \bold{Numeric}\cr
#'   [, 4] \tab charge \tab Charge time (in hours) \tab \bold{Numeric}\cr
#'   [, 5] \tab recharge \tab Level of recharge \tab \bold{Numeric}\cr
#'   [, 6] \tab koh_percent \tab Concentration of Potassium Hydroxide (as percent) \tab \bold{Numeric}\cr
#'   [, 7] \tab koh_volume \tab Volume of Potassium Hydroxide (in cubic-centimeters) \tab \bold{Numeric}\cr
#'   [, 8] \tab precharge \tab Precharge time (in hours) \tab \bold{Numeric}\cr
#'   [, 9] \tab cycles \tab Accumulated cycles at \code{event} \tab \bold{Numeric}\cr
#'   [,10] \tab event \tab Event observed at \code{cycles} (failure/right-censored)  \tab \bold{Categoric}
#'    }
#' @source Brown, H. M., and Mains D. E (1979)
#'         Accelerated Test Program for Sealed Nickel--Cadmium Spacecraft Batteries/Cells,
#'         Technical Report WQEC/C 79-145. Available from the Department of the Navy, 
#'         Naval Weapons Support Center, Weapons Quality Engineering Center, Crane, IN 47522.
#' @description Brown and Mains (1979) present the results of an extensive experiment to evaluate 
#'              the long-term performance of rechargable nickel-cadmium battery cells that 
#'              were to be used in spacecraft. The study used 8 experimental factors. The 
#'              first five factors are environmental or accelerating factors (set to higher 
#'              than usual levels to obtain failure information more quickly).  The other 
#'              three factors were product-design factors that could be adjusted in the product
#'              design to optimize performance and reliability of the batteries to be manufactured.  
#'              The experiment ran 82 batteries, each containing 5 individual cells.  Each battery 
#'              was tested at a combination of factor levels determined according to a central 
#'              composite experimental plan.
NULL

#' ##### 
#' 
#' @docType data
#' @family data-notdone
#' @name parta   
#' @format A \code{data.frame} with 60 rows and 2 variables:
#' \tabular{rlll}{
#'   [, 1] \tab kilocycles \tab Accumulated cycles at failure (in thousands) \tab \bold{Numeric}\cr
#'   [, 2] \tab operator \tab Test operator \tab \bold{Categoric}
#'    }
#' @source Meeker, W. Q. (1999) An experiment to compare the life times of units assembled by three	different	operators (unpublished)
#' @description ####
NULL

#' #####
#' 
#' @docType data
#' @name photodetector   
#' @format A \code{data.frame} with 7 rows and 4 variables:
#' \tabular{rlll}{
#'   [, 1] \tab lower \tab Time (in hours) at which an inspection interval began \tab \bold{Numeric}\cr
#'   [, 2] \tab upper \tab Time (in hours) at which an inspection interval ended \tab \bold{Numeric}\cr
#'   [, 3] \tab event \tab Event observed in the interval (right-censored/interval-censored) \tab \bold{Categoric}\cr
#'   [, 4] \tab count \tab Number of events observed in the interval \tab \bold{Numeric}
#'    }
#' @source Weis	et al. (1986)
#' @description ####
NULL

#' Tensile Fatigue Test of Polyester/Viscose Yarn 
#'
#' @docType data
#' @family data-notdone
#' @rdname piccioto
#' @name piccioto
#' @name piccioto2
#' @name piccioto3   
#' @format A \code{data.frame} with 797 rows and 3 variables:
#' \tabular{rlll}{
#'   [, 1] \tab inches \tab Length of the test specimen \tab \bold{Numeric}\cr
#'   [, 2] \tab kilocycles \tab Accumulated cycles at \code{event} (in thousands) \tab \bold{Numeric}\cr
#'   [, 3] \tab event \tab Event observed at \code{kilocycles} (failed/right-censored) \tab \bold{Categoric}
#'    }
#' @source Picciotto R., (1970) 
#'         Tensile Fatigue Characteristics of a Sized Polyester/Viscose Yarn and Their Effect on Weaving Performance. 
#'         A thesis submitted to the Graduate Faculty of North Carolina State University at Raleigh in partial fulfillment of the requirements for the Degree of Master of Science. 
#'         Department of Textile Technology.
#' @description Picciotto includes the \code{length} and \code{kcycles} columns.  
#'              picciotto2 is a subset of the picciotto data set, including only the units for which \code{length = 30,60, or 90}.  
#'              picciotto3 mirrors picciotto2 but assumes that the test conlcuded at 100 kilocycles
NULL

#' ##### 
#'
#' @docType data
#' @family data-notdone
#' @name pipelinethickness   
#' @format A \code{data.frame} with 200 rows and 1 variable:
#' \tabular{rlll}{
#'   [, 1] \tab inches \tab Observed pipe thickness \tab \bold{Numeric}
#'    }
#' @source ####
#' @description ####
NULL

#' ##### 
#' 
#' @docType data
#' @family data-notdone
#' @name pmax   
#' @format A \code{data.frame} with 170 rows and 9 variables:
#' \tabular{rlll}{
#'   [, 1] \tab penid \tab Identification designator \tab \bold{Categoric}\cr
#'   [, 2] \tab batch \tab Batch indicator \tab \bold{Categoric}\cr
#'   [, 3] \tab celsius \tab Temperature applied \tab \bold{Numeric}\cr
#'   [, 4] \tab days \tab Accumulated time at \code{event} \tab \bold{Numeric}\cr
#'   [, 5] \tab sample \tab Type of sample tested \tab \bold{Categoric}\cr
#'   [, 6] \tab wafer \tab Type of wafer \tab \bold{Categoric}\cr
#'   [, 7] \tab pmax \tab Maximum value of p observed \tab \bold{Numeric}\cr
#'   [, 8] \tab locus \tab Type of locus \tab \bold{Categoric}\cr
#'   [, 9] \tab event \tab Event observed at \code{days} (failure/right-censored) \tab \bold{Categoric}
#'    }
#' @source ####
#' @description ####
NULL

#' #####
#'
#' @docType data
#' @name primarybattery   
#' @format A \code{data.frame} with 7 rows and 4 variables:
#' \tabular{rlll}{
#'   [, 1] \tab lower \tab Start of an inspection interval (in days) \tab \bold{Numeric}\cr
#'   [, 2] \tab upper \tab End of an inspection interval (in days) \tab \bold{Numeric}\cr
#'   [, 3] \tab event \tab Event observed in the interval (right-censored/interval-censored)  \tab \bold{Categoric}\cr
#'   [, 4] \tab count \tab Number of events observed in the interval \tab \bold{Numeric}
#'    }
#' @source louis	hart	(1987),	IEEE	Rel	5-10
#' @description ####
NULL

#' Printed circuit board accelerated life test 
#'
#' @docType data
#' @name printedcircuitboard   
#' @format A \code{data.frame} with 140 rows and 5 variables:
#' \tabular{rlll}{
#'   [, 1] \tab lower \tab Start of an inspection interval (in hours) \tab \bold{Numeric}\cr
#'   [, 2] \tab upper \tab End of an inspection interval (in hours) \tab \bold{Numeric}\cr
#'   [, 3] \tab count \tab Number of events observed in the interval \tab \bold{Numeric}\cr
#'   [, 4] \tab event \tab Event observed in the interval (right-censored/interval-censored) \tab \bold{Categoric}\cr
#'   [, 5] \tab rh    \tab Relative humidity applied \tab \bold{Numeric}
#'    }
#' @source Meeker, W. Q., and LuValle, M. J. (1995), 
#'         An accelerated life test model based on reliability kinetics, 
#'         Technometrics, \bold{37}, 133-146.
#' @description Meeker and LuValle (1995) give data from an accelerated life test on failure of 
#'              printed circuit boards. The purpose of the experiment was to study the 
#'              effect of the stresses on the failure-time distribution and to predict 
#'              reliability under normal operating conditions. More specifically, the 
#'              experiment was designed to study a particular failure mode - the formation 
#'              and growth of conductive anodic filaments between copper-plated through-holes 
#'              in the printed circuit boards.  Actual growth of the filaments could not be 
#'              monitored, only failure time (defined as a short circuit) could be observed 
#'              directly.  Special test boards were constructed for the experiment. 
#'              
#'              The data provided here are the number of failures observed in each of a series of 
#'              4-hour and 12-hour long intervals over the life-test period.  This experiment resulted 
#'              in interval-censored data because only the interval in which each failure occurred was 
#'              known.  Further, the data are part of the results of a much larger experiment aimed at 
#'              determining the effects of temperature, relative humidity, and electric field 
#'              on the reliability of printed circuit boards.
NULL

#' ##### 
#'
#' @docType data
#' @family data-notdone
#' @name prob3_5   
#' @format A \code{data.frame} with 25 rows and 2 variables:
#' \tabular{rlll}{
#'   [, 1] \tab kilocycles \tab Accumulated cycles at \code{event} (in thousands) \tab \bold{Numeric}\cr
#'   [, 2] \tab event \tab Event observed at \code{kcycles} (failure/right-censored) \tab \bold{Categoric}
#'    }
#' @source ####
#' @description ####
NULL

#' ##### 
#' 
#' @docType data
#' @name pulse   
#' @format A \code{data.frame} with 20 rows and 2 variables:
#' \tabular{rlll}{
#'   [, 1] \tab count \tab Number of events observed at \code{pulse} \tab \bold{Numeric}\cr
#'   [, 2] \tab pulse \tab Pulse level observed \tab \bold{Numeric}\cr
#'    }
#' @source ####
#' @description ####
NULL

#' Car door lock transmitter replacement data 
#' 
#' @docType data
#' @family data-almost
#' @name r4490   
#' @format A \code{data.frame} with 47 rows and 4 variables:
#' \tabular{rlll}{
#'   [, 1] \tab vin \tab Vehicle identifcation number \tab \bold{Categoric}\cr
#'   [, 2] \tab days \tab Accumulated time at \code{event} \tab \bold{Numeric}\cr
#'   [, 3] \tab event \tab Event observed at \code{days} (R4490/End/MEnd) \tab \bold{Categoric}\cr
#'   [, 4] \tab costcount \tab What is cost count? \tab \bold{Numeric}
#'    }
#' @source ####
#' @description R4490 is the code used by General Motors denoting Remote Control Door Lock Transmitter Replacement
NULL

#' ##### 
#'
#' @docType data
#' @family data-almost
#' @name repairtimes   
#' @format A \code{data.frame} with 119 rows and 1 variable:
#' \tabular{rlll}{
#'   [, 1] \tab hours \tab Time required to complete a repair \tab \bold{Numeric}\
#'    }
#' @source Meeker, W. Q., (1997) Recorded times to repair a particular kind of electronic system, without regard to failure	mode (unpublished)
#' @description ####
NULL

#' Carbon-Film Resistor Accelerated Degradation Test 
#'
#' @docType data
#' @name resistor   
#' @format A \code{data.frame} with 116 rows and 4 variables:
#' \tabular{rlll}{
#'   [, 1] \tab percent \tab Observed increase in resistance \tab \bold{Numeric}\cr
#'   [, 2] \tab resistor \tab Resistor type \tab \bold{Categoric}\cr
#'   [, 3] \tab celsius \tab Temperature applied to \code{resistor} \tab \bold{Numeric}\cr
#'   [, 4] \tab hours \tab Accumulated hours (in thousands) when \code{percent} was measured\tab \bold{Numeric}
#'    }
#' @source Shiomi, H., and Yanagisawa, T. (1979), 
#'         On distribution parameter during accelerated life test for a carbon film resistor, 
#'         Bulletin of the Electrotechnical Laboratory, \bold{43}, 330-345 (in Japanese).
#' @source Suzuki, K., Maki, K., and Yokogawa, S. (1993), 
#'         An analysis of degradation data of a carbon film and properties of the estimators, 
#'         in Statistical Sciences and Data Analysis, 501-511. K. Matusita, M. Puri, and T. Hayakawa, Editors.
#'         Utrecht, Netherlands: VSP.
#' @description Samples of carbon-film resistors were tested at each of three levels of temperature. At the standard 
#'              operating temperature of 50^{o} C, carbon-film resistors will slowly degrade. Changes in resistance 
#'              can cause reduced product performance or even cause system failures.  The test was run at high levels 
#'              of temperature to accelerate the chemical degradation process and obtain degradation data more quickly.
#'              
#'              This dataset presents the percent change in resistance measured throughout the test, 
#'              while the \code{\link{resistor2}} dataset presents the absolute value of resistance measure during the test.
#' @seealso \code{\link{resistor2}}
NULL

#' Carbon-Film Resistor Accelerated Degradation Test
#'
#' @docType data
#' @name resistor2
#' @format A \code{data.frame} with 145 rows and 4 variables:
#' \tabular{rlll}{
#'   [, 1] \tab resistor \tab Resistor type \tab \bold{Categoric}\cr
#'   [, 2] \tab celsius \tab Temperature applied to the resistor  \tab \bold{Numeric}\cr
#'   [, 3] \tab hours \tab Accumulated time when \code{resistance} was measured \tab \bold{Numeric}\cr
#'   [, 4] \tab resistance \tab Resistance (ohms) observed \tab \bold{Numeric}
#'    }
#' @source Shiomi, H., and Yanagisawa, T. (1979), 
#'         On distribution parameter during accelerated life test for a carbon film resistor, 
#'         Bulletin of the Electrotechnical Laboratory, \bold{43}, 330-345 (in Japanese).
#' @source Suzuki, K., Maki, K., and Yokogawa, S. (1993), 
#'         An analysis of degradation data of a carbon film and properties of the estimators, 
#'         in Statistical Sciences and Data Analysis, 501-511. K. Matusita, M. Puri, and T. Hayakawa, Editors.
#'         Utrecht, Netherlands: VSP.
#' @description Samples of carbon-film resistors were tested at each of three levels of temperature. At the standard 
#'              operating temperature of 50^{o} C, carbon-film resistors will slowly degrade. Changes in resistance 
#'              can cause reduced product performance or even cause system failures.  The test was run at high levels 
#'              of temperature to accelerate the chemical degradation process and obtain degradation data more quickly.
#'              
#'              The \code{\link{resistor}} dataset presents the percent change in resistance measured throughout the test, 
#'              while this dataset presents the absolute value of resistance measure during the test.
#' @seealso \code{\link{resistor}}
NULL

#' ##### 
#'
#' @docType data
#' @family data-notdone
#' @name riverchem
#' @format A \code{data.frame} with 8 rows and 4 variables:
#' \tabular{rlll}{
#'   [, 1] \tab obs \tab Observation number \tab \bold{Numeric}\cr
#'   [, 2] \tab qtr \tab Quarter when observation was made (Q1-Q4) \tab \bold{Categoric}\cr
#'   [, 3] \tab day \tab Day when observation was made (Monday-Friday) \tab \bold{Categoric}\cr
#'   [, 4] \tab ppm \tab Observed chemical concentration (in parts-per-million) \tab \bold{Numeric}
#'    }
#' @source Meeker, W. Q. and Escobar, L. A. (1998) Statistical Methods for Reliability Data, New York, NY; John Wiley & Sons.
#' @description ####
NULL

#' ##### 
#'
#' @docType data
#' @family data-almost
#' @name rocketmotor
#' @format A \code{data.frame} with 19 rows and 3 variables:
#' \tabular{rlll}{
#'   [, 1] \tab years \tab Accumulated time at \code{event} \tab \bold{Numeric}\cr
#'   [, 2] \tab event \tab Event observed at \code{years} (right-censored/left-censored)  \tab \bold{Categoric}\cr
#'   [, 3] \tab count \tab Number of events observed at \code{years} \tab \bold{Numeric}
#'    }
#' @source Olwell, D. H. and Sorell, A. A. (2001), Proceedings of the 2001 Annual Reliability and Maintanability Symposium.
#' @description ####
NULL

#' ##### 
#'
#' @docType data
#' @family data-notdone
#' @name shelflifea
#' @format A \code{data.frame} with 112 rows and 5 variables:
#' \tabular{rlll}{
#'   [, 1] \tab percent \tab Initial concentration level \tab \bold{Numeric}\cr
#'   [, 2] \tab celsius \tab Temperature applied \tab \bold{Numeric}\cr
#'   [, 3] \tab days \tab Accumulated time when \code{percent} measured \tab \bold{Numeric}\cr
#'   [, 4] \tab truntime \tab Truncation time (in days) \tab \bold{Numeric}\cr
#'   [, 5] \tab truntype \tab Truncation type (right-truncated) \tab \bold{Categoric}
#'    }
#' @source ####
#' @description ####
NULL

#' Vehicle shock absorber failure data
#'
#' @docType data
#' @family data-almost
#' @name shockabsorber
#' @format A \code{data.frame} with 38 rows and 3 variables:
#' \tabular{rlll}{
#'   [, 1] \tab kilometers \tab Accumulated distance at \code{event} \tab \bold{Numeric}\cr
#'   [, 2] \tab mode       \tab Failure mode observed at \code{miles} \tab \bold{Categoric}\cr
#'   [, 3] \tab event      \tab Event observed at \code{miles} (failure/right-censored) \tab \bold{Categoric}
#'    }
#' @source O'Connor, P. D. T. (1985), 
#'         Practical Reliability Engineering (Second Edition), New York, NY; John Wiley & Sons.
#' @description O'Connor gives the failure times (in number of kilometers of use) of vehicle shock absorbers. 
#'              The data shows two different failure modes occurring, denoted by M1 and M2. Engineers responsible 
#'              for shock absorber manufacturing and reliability were interested in the distribution of kilometers 
#'              to failure for the individual failure modes. Engineers responsible for higher-level automobile system 
#'              reliability and choosing among alternative vendors were interested in the overall failure distribution 
#'              for the shock absorbers.
NULL

#' Life test comparing different snubber designs
#'
#' @docType data
#' @family data-almost
#' @name snubber
#' @format A \code{data.frame} with 51 rows and 4 variables:
#' \tabular{rlll}{
#'   [, 1] \tab cycles \tab Accumulated cycles at \code{event} \tab \bold{Numeric}\cr
#'   [, 2] \tab event \tab Event observed at \code{cycles} (failure/right-censored)  \tab \bold{Categoric}\cr
#'   [, 3] \tab count \tab Number of events observed at \code{cycles} \tab \bold{Numeric}\cr
#'   [, 4] \tab design \tab Design type (Old/New) \tab \bold{Categoric}
#'   }
#' @source Nelson, W. (1982), Applied Life Data Analysis, pg. 529, New York, NY; John Wiley \& Sons.
#' @description A snubber is a component in an electric toaster. Nelson presents data from a life 
#'              test comparing two different snubber designs.
#'              
NULL

#' Nickel-based Superalloy Fatigue Test
#'
#' @docType data
#' @name superalloy
#' @format A \code{data.frame} with 23 rows and 6 variables:
#' \tabular{rlll}{
#'   [, 1] \tab kilocycles \tab Accumulated cycles at \code{event} (in thousands) \tab \bold{Numeric}\cr
#'   [, 2] \tab event \tab Event observed at \code{kcycles} (failure/right-censored) \tab \bold{Categoric}\cr
#'   [, 3] \tab count \tab Number of events observed at \code{kcycles} \tab \bold{Numeric}\cr
#'   [, 4] \tab pstress \tab Pseudo-stress applied derated \tab \bold{Numeric}\cr
#'   [, 5] \tab lpstress \tab Log[pseudo-stress] (\code{ln[pstress]})The log base e transformation of the Pseudo-Stress \tab \bold{Numeric}\cr
#'   [, 6] \tab lpstress2 \tab Square of log[pseudo-stress] (\code{ln[pstress]^2}) \tab \bold{Numeric}
#'   }
#' @source Nelson, W. (1990), 
#'         Accelerated Testing: Statistical Models, Test Plans, and Data Analyses, 
#'         New York, NY; John Wiley \& Sons.
#' @description Nelson (1990) presents and analyzes life data from a strain-controlled, low-cycle fatigue test performed on 26 cylindrical 
#'              specimens of a nickel-base superalloy. Four of the specimens were removed from the test before failure.
#'              In addition to recording the number of cycles to failure, the level of pseudostress (Young's modulus 
#'              times strain) was also measured.  The initial purpose of Nelson's analysis was to estimate the curve 
#'              giving the number of cycles at which .1\% of the population of such specimens would fail, as a function 
#'              of pseudostress.
NULL

#' Electrolytic capacitor accelerated life test data
#'
#' @docType data
#' @name tantalum
#' @format A \code{data.frame} with 48 rows and 5 variables:
#' \tabular{rlll}{
#'   [, 1] \tab hours \tab Accumulated time at \code{event} \tab \bold{Numeric}\cr
#'   [, 2] \tab event \tab Event observed at \code{hours} (failure/right-censored) \tab \bold{Categoric}\cr
#'   [, 3] \tab count \tab Number of events observed at \code{hours} \tab \bold{Numeric}\cr
#'   [, 4] \tab volts \tab Voltage applied to the unit \tab \bold{Numeric}\cr
#'   [, 5] \tab celsius \tab Temperature applied to the unit \tab \bold{Numeric}
#'    }
#' @source Singpurwalla, N. D., Castellino, V. C., and Goldschen, D. Y. (1975),
#'         Inference from accelerated life tests using Eyring type re-parameterizations,  
#'         Naval Research Logistics Quarterly, \bold{22}, 289-296.
#' @description Singpurwalla, Castellino, and Goldschen (1975) present temperature/voltage accelerated life test data on tantalum 
#'              electrolytic capacitors. The tests were conducted at temperature/voltage combinations that were nonrectangular 
#'              and with unequal allocations of units.
NULL

#' #####
#'
#' @docType data
#' @family data-notdone
#' @name testdadtplan
#' @format A \code{data.frame} with 9 rows and 3 variables:
#' \tabular{rlll}{
#'   [, 1] \tab celsius \tab Temperature applied to the unit \tab \bold{Numeric}\cr
#'   [, 2] \tab days \tab Event time \tab \bold{Numeric}\cr
#'   [, 3] \tab count \tab Number of units tested \tab \bold{Categoric}
#'    }
#' @source Shimokawa, T., and Hamaguchi, Y. (1987), Statistical Evaluation of Fatigue Life and Fatigue Strength in Circular-Holed Notched Specimens of a Carbon Eight-Harness-Satin/Epoxy Laminate,'' in Statistical Research on Fatigue and Fracture (Current Japanese Materials Research, Vol. 2), eds. T. Tanaka, S. Nishijima, and M. Ichikawa, London: Elsevier, pp. 159-176.
#' @description ####
NULL

#' #####
#'
#' @docType data
#' @family data-notdone
#' @name titanium
#' @format A \code{data.frame} with 96 rows and 3 variables:
#' \tabular{rlll}{
#'   [, 1] \tab kilocycles \tab Accumulated cycles at \code{event} (in thousands) \tab \bold{Numeric}\cr
#'   [, 2] \tab strain \tab Strain measured at \code{kilocycles} \tab \bold{Numeric}\cr
#'   [, 3] \tab event \tab Event observed at \code{kilocycles} (failure/right-censored)  \tab \bold{Categoric}
#'    }
#' @source Meeker W.Q., Escobar L.A., and Lu (1998)
#' @description ####
NULL

#' Titanium fatigue crack growth data
#'
#' @docType data
#' @family data-notdone
#' @name titanium2
#' @format  A \code{data.frame} with 10 rows and 3 variables:
#' \tabular{rlll}{
#'   [, 1] \tab kilocycles \tab Accumulated cycles at \code{event} \tab \bold{Numeric}\cr
#'   [, 2] \tab event      \tab Event observed at \code{cycles} (failure/right-censored) \tab \bold{Categoric}\cr 
#'   [, 3] \tab count      \tab Number of events observed at \code{cycles} \tab \bold{Numeric}
#'   }
#' @source Meeker, W. Q. and Escobar, L. A. (1998) Statistical Methods for Reliability Data, New York, NY; John Wiley & Sons.
#' @source Hudak, S.J., Saxena, A., Bucci, R. J., and Malcom, R .C. (1978), 
#'         Development of standard methods of testing and analyzing fatigure crack growth rate data, 
#'         Technical Report AFML-TR-78-40, Westinghouse R & D Center, Westinghouse Electric Corporation, Pittsburgh, PA.
#' @description A sample of 100 specimens of a titanium alloy were subjected to a fatigue test to determine time 
#'              to crack initiation. The test was run up to a limit of 100,000 cycles.  The observed number of cycles to crack 
#'              initiation (in units of 1,000 of cycles) were: 18, 32, 39, 53, 59, 68, 77, 78, 93. No crack had initiated in 
#'              any of the other 91 other specimens prior to reaching 100,000 cycles.
NULL

#' ##### 
#'
#' @docType data
#' @family data-notdone
#' @name tractorbreaks    
#' @format A \code{data.frame} with 107 rows and 1 variable:
#' \tabular{rlll}{
#'   [, 1] \tab hours \tab Accumulated time at failure \tab \bold{Numeric}
#'    }
#' @source ####
#' @description ####
NULL

#' #####
#'
#' @docType data
#' @family data-almost
#' @name tree25years
#' @format A \code{data.frame} with 29 rows and 1 variable:
#' \tabular{rlll}{
#'   [, 4] \tab meters \tab Annual growth in tree height \tab \bold{Numeric}
#'    }
#' @source Meeker W.Q. and Escobar L.A. (1998) Statistical Methods for Reliability Data, New York, NY; John Wiley & Sons.
#' @description ####
NULL

#' Turbine wheel crack initiation data
#'
#' @docType data
#' @name turbine
#' @format A \code{data.frame} with 21 rows and 3 variables:
#' \tabular{rlll}{
#'   [, 1] \tab hours \tab Accumulated hours (in hundreds) at \code{event} \tab \bold{Numeric}\cr
#'   [, 2] \tab event \tab Event observed at \code{hours} (right-censored/left-censored) \tab \bold{Categoric}\cr
#'   [, 3] \tab count \tab Number of events observed at \code{hours} \tab \bold{Numeric}
#'    }
#' @source Nelson, W.	(1982) Applied Life Data Analysis, New York, NY: John Wiley \& Sons.
#' @description Nelson (1982) describes a study to estimate the 
#'              distribution of time to crack initiation for turbine 
#'              wheels.  Each of 432 wheels were inspected once to 
#'              determine if it had started to crack or not.  At the 
#'              time of the inspections, the wheels had different 
#'              amounts of service time (age). A unit found to be 
#'              cracked at its inspection was labelled as left-censored 
#'              at its age (because the crack had initiated at some 
#'              unknown point before its inspection age).  A unit found 
#'              to be uncracked at its inspection was labelled as 
#'              right-censored at its age (because a crack would be 
#'              initiated at some unknown point after that age). 
#'              The data show the number of cracked and uncracked 
#'              wheels in different age categories, showing the midpoint
#'              of the time interval given by Nelson.  The data were 
#'              put into intervals to facilitate simpler analyses.
NULL

#' #####
#'
#' @docType data
#' @family data-notdone
#' @name turbinedevice
#' @format A \code{data.frame} with 50 rows and 2 variables:
#' \tabular{rlll}{
#'   [, 1] \tab cycles \tab Accumulated cycles (in millions) at failure \tab \bold{Numeric}\cr
#'   [, 2] \tab mode \tab Failure mode observed at \code{mcycles} (Crack/Fixture) \tab \bold{Categoric}
#'    }
#' @source Unpublished Meeker (1999)
#' @description ####
NULL

#' Transmitter vacuum tube life test data
#'
#' @docType data
#' @name v7tube
#' @format A \code{data.frame} with 5 rows and 4 variables:
#' \tabular{rlll}{
#'   [, 1] \tab lower \tab Start of an inspection interval (in days) \tab \bold{Numeric}\cr
#'   [, 2] \tab upper \tab End of an inspection interval (in days) \tab \bold{Numeric}\cr
#'   [, 3] \tab event \tab Event observed in the interval (right-censored/left-censored/interval-censored)  \tab \bold{Categoric}\cr
#'   [, 4] \tab count \tab Number of events observed in the interval \tab \bold{Numeric}
#'    }
#' @source Davis, D. J. (1952) An analysis of some failure data, 
#'         Journal of the American Statistical Association, \bold{47}, 113-150.
#' @description Although solid-state electronics have made vacuum tubes obsolete for most applications, such tubes are still 
#'              widely used in the output stage of high-power transmitters. Davis (1952) presents life data for a certain kind 
#'              of transmitter vacuum tube (designated as "V7" within a particular transmitter design).  For this dataset, and in 
#'              many practical situations, the exact failure times were not reported. Instead the data only contain the number of 
#'              failures observed within each inspection interval.
NULL

#' #####
#'
#' @docType data
#' @family data-almost
#' @name v805tube
#' @format A \code{data.frame} with 18 rows and 4 variables:
#' \tabular{rlll}{
#'   [, 1] \tab lower \tab Start of an inspection interval (in days) \tab \bold{Numeric}\cr
#'   [, 2] \tab upper \tab End of an inspection interval (in days) \tab \bold{Numeric}\cr
#'   [, 3] \tab event \tab Event observed in the interval (right-censored/left-censored/interval-censored)  \tab \bold{Categoric}\cr
#'   [, 4] \tab count \tab Number of events observed in the interval \tab \bold{Numeric}
#'    }
#' @source Davis, D. J. (1952) An analysis of some failure data, 
#'         Journal of the American Statistical Association, \bold{47}, 113-150.
#' @description ####
NULL

#' Diesel engine valve seat data
#'
#' @docType data
#' @name valveseat
#' @format A \code{data.frame} with 89 rows and 3 variables:
#' \tabular{rlll}{
#'   [, 1] \tab engine \tab Designator code specifying each engine under test \tab \bold{Categoric}\cr
#'   [, 2] \tab days   \tab Accumulated time at \code{event} \tab \bold{Numeric}\cr
#'   [, 3] \tab event  \tab Event observed at \code{days} (replacement/end) \tab \bold{Categoric}\cr
#'   }
#' @source Nelson, W. and Doganaksoy, N. (1989)  
#'         A computer program for an estimate and confidence limits for the mean cumulative function for cost or number of repairs of
#'         repairable products, 
#'         TIS report 89CRD239, General Electric Company Research and Development, Schenectady, NY.
#' @source Nelson, W. (1995) Confidence limits for recurrence data - applied to cost or number of product repairs, 
#'         Technometrics, \bold{37}, 147-157.
#' @description Nelson and Doganaksoy report the age when a valve seat replacement occurred across a fleet of 41 diesel 
#'              engines. For each engine, the number of days when a replacement event occured were recorded along with the number of 
#'              days when the observational period ended. The data were recorded to answer the following questions:
#'              \describe{ 
#'              {1}{Does the replacement rate increased with age?}
#'              {2}{How many replacement valves will be needed in some future period of time?}
#'              {3}{Can valve life in these systems be modeled as a superimposed renewal process?}
#'              }
NULL

#' #####
#'
#' @docType data
#' @family data-notdone
#' @name vehiclemotor
#' @format A \code{data.frame} with 43 rows and 3 variables:
#' \tabular{rlll}{
#'   [, 1] \tab hours \tab Accumulated time at \code{event} \tab \bold{Numeric}\cr
#'   [, 3] \tab event \tab Event observed at \code{hours} (failure/right-censored)  \tab \bold{Categoric}\cr
#'   [, 4] \tab count \tab Number of events observed at \code{hours} \tab \bold{Numeric}
#'    }
#' @source ####
#' @description ####
NULL

#' #####
#'
#' @docType data
#' @family data-notdone
#' @name voltageendurance
#' @format A \code{data.frame} with 58 rows and 2 variables:
#' \tabular{rlll}{
#'   [, 1] \tab hours \tab Accumulated time at \code{event} \tab \bold{Numeric}\cr
#'   [, 2] \tab event \tab Event observed at \code{hours} (ProcessDefect/Degradation/Right-censored) \tab \bold{Categoric}
#'    }
#' @source Meeker W.Q. and Escobar L.A. (1998) Statistical Methods for Reliability Data, New York, NY; John Wiley & Sons.
#' @description ####
NULL

#' #####
#'
#' @docType data
#' @family data-notdone
#' @name workstation
#' @format A \code{data.frame} with 12 rows and 3 variables:
#' \tabular{rlll}{
#'   [, 1] \tab station \tab Station where \code{event} was observed \tab \bold{Categoric}\cr
#'   [, 2] \tab days \tab Accumulated time at \code{event} \tab \bold{Numeric}\cr
#'   [, 3] \tab event \tab Event observed at \code{days} (report/end) \tab \bold{Categoric}
#'    }
#' @source Meeker and Escobar (1998)
#' @description ####
NULL

#' Glass capacitor accelerated life test data
#'
#' @docType data
#' @name zelencap
#' @format A \code{data.frame} with 40 rows and 5 variables:
#' \tabular{rlll}{
#'   [, 1] \tab hours \tab Accumulated time at \code{event} \tab \bold{Numeric}\cr
#'   [, 2] \tab event \tab Event observed at \code{hours} (failure/right-censored) \tab \bold{Categoric}\cr
#'   [, 3] \tab count \tab Number of events observed at \code{hours} \tab \bold{Numeric}\cr
#'   [, 4] \tab celsius \tab Temperature applied  \tab \bold{Numeric}\cr
#'   [, 5] \tab volts \tab Electrical power applied \tab \bold{Numeric}
#'   }
#' @source Zelen, M. (1959) Factorial experiments in life testing, Technometrics, \bold{1}3, 269-288
#' @description Zelen describes an accelerated life test performed on glass capacitors at higher than usual levels of 
#'              temperature and voltage. The data were obtained from a factorial experiment in which eight capacitors were tested 
#'              at each combination of temperature and voltage. For each combination, the test was terminated after the fourth failure
#'              was observed, yielding failure (Type II) censored data.
NULL
