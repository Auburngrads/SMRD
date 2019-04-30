library(smrdfortran)
data.rdu <- frame.to.rdu(r4490,
                         time.column = 2,
                         cost.count.column = 4,
                         ID.column = 1,
                         event.column = 3)
kdebug1 = F
JustEvent = T

time.column <- attr(data.rdu, "time.column")
event.column <- attr(data.rdu, "event.column")
WindowInfo <- attr(data.rdu, "WindowInfo")
event <- data.rdu[, event.column]
Times <- data.rdu[, time.column]
EndPoints <- is.element(casefold(event), c("end", "mend"))
StartPoints <- is.element(casefold(event), c("start", "mstart"))
Cevent <- !(EndPoints | StartPoints)

  if (JustEvent) {

    tuniq <- unique(sort(Times[Cevent]))

    } else {  tuniq <- unique(sort(c(0, Times[Cevent], WindowInfo$WindowL,
                                WindowInfo$WindowU)))
    }

oldrs <- .Fortran("riskset", muniqrecurr = as.integer(length(tuniq)),
                 tuniq = as.double(tuniq), nwindows = as.integer(length(WindowInfo$WindowU)),
                 twindowsl = as.double(WindowInfo$WindowL), twindowsu = as.double(WindowInfo$WindowU),
                 wcounts = as.integer(WindowInfo$WindowCounts), iordl = integer(length(WindowInfo$WindowL)),
                 iordu = integer(length(WindowInfo$WindowL)), delta = integer(length(tuniq)),
                 kdebug1= as.integer(kdebug1), iscrat = integer(length(WindowInfo$WindowL)))

newrs <- SMRD2::RISKSET(muniqrecurr = as.integer(length(tuniq)),
                             tuniq = as.double(tuniq), 
                             nwindows = as.integer(length(WindowInfo$WindowU)),
                             twindowsl = as.double(WindowInfo$WindowL), 
                             twindowsu = as.double(WindowInfo$WindowU),
                             wcounts = as.integer(WindowInfo$WindowCounts), 
                             iordl = integer(length(WindowInfo$WindowL)),
                             iordu = oldrs$iordu - 1, 
                             delta = integer(length(tuniq)),
                             kdebug= as.integer(kdebug1), 
                             iscrat = integer(length(WindowInfo$WindowL)))

twindowsl = as.double(WindowInfo$WindowL)
twindowsu = as.double(WindowInfo$WindowU)

df <- data.frame(valuesL = twindowsl,
                 fort_L = oldrs$iordl,
                 cpp_L = newrs$iordl,
                 R_L = rank(twindowsl),
                 valuesU = twindowsu,
                 fort_U = oldrs$iordu,
                 cpp_U = newrs$iordu,
                 R_U = rank(twindowsu))

df
