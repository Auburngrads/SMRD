print.mttf <-
function (x,...)
{
    if (x$fun.hat == Inf) {
      mean.text<-paste("The ML estimate of mean time to failure (MTTF) for the ",x$title, " (assuming the ",
              x$distribution, " distribution is correct) does not exist (is infinite).",  sep = "")

    } else {
      mean.text<-paste(paste("The ML estimate of mean time to failure (MTTF) for the", x$title, "(assuming the",
               x$distribution, "distribution is correct) was found to be", format(x$fun.hat),
               x$time.units, sep = " "), ". An approximate ", percent.conf.level(x$conf.level),
               " confidence interval on the MTTF is [",  format(x$fun.lower), ", ", format(x$fun.upper),"].", sep = "")

    }

  print(mean.text)
      }
