getPlanString <-
function (RMD.plan, for.name = F) 
{
    sample.sizes <- RMD.plan$sample.sizes
    number.measurments <- lapply(RMD.plan$time.vectors, length)
    if (for.name) 
        return(paste("n", sample.sizes, "m", number.measurments, 
            sep = "", collapse = "."))
    else return(paste(sample.sizes, "(", number.measurments, 
        ")", sep = "", collapse = ","))
}
