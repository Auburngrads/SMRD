get.old.par <-
function () 
{
    old.par <- par()
    if (exists("is.R") & is.R()) {
        old.par$cin <- NULL
        old.par$cra <- NULL
        old.par$csi <- NULL
        old.par$cxy <- NULL
        old.par$din <- NULL
    }
    old.par
}
