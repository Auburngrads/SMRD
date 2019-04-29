FixHowcdfTimesList <-
function (HowcdfTimesList, cdfTimesList) 
{
    switch(HowcdfTimesList, Automatic = {
        cdfTimesList <- cdfTimesList
    }, `List numbers` = {
        cdfTimesList <- cdfTimesList
    }, `S-Plus expression` = {
        tmp <- eval(parse(text = cdfTimesList))
        cdfTimesList <- paste(paste(tmp), collapse = ",")
    })
    return(cdfTimesList)
}
