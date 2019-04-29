FixHowSubset <-
function (func.call) 
{
    switch(func.call$how.subset, `Use all data` = {
        func.call$subset <- T
    }, `S-Plus expression` = {
        cat("\nSubsetting data by", func.call$subset, "\n")
    })
    return(func.call)
}
