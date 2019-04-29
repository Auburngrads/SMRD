probplot.setup.title <-
function (title.option = GetSMRDDefault("SMRD.TitleOption"), my.title = "my.title", sub.title = "subtitle", 
    distribution = "Weibull", shape = NULL) 
{
    if (is.null(shape)) 
        shape.title <- ""
    else {
        if (length(shape == 1)) 
            shape.title <- paste("with shape parameter=", paste(shape))
        else {
            shape.title <- paste("with shape parameters=", paste(shape))
        }
    }
    cut.top <- T
    switch(title.option, full = {
        new.title <- paste(my.title, "\n", distribution, "Probability Plot", 
            shape.title, sub.title)
    }, only.dist = {
        cut.top <- F
        new.title <- paste(distribution, "Probability Plot", 
            shape.title, sub.title)
    }, paper = {
        cut.top <- T
        new.title <- paste(distribution, "Probability Scale", 
            shape.title, sub.title)
    }, paper2 = {
        cut.top <- T
        new.title <- ""
    }, only.my = {
        cut.top <- T
        new.title <- my.title
    }, cut.sub = {
        cut.top <- T
        new.title <- sub.title
    }, only.sub = {
        cut.top <- F
        new.title <- sub.title
    }, blank = {
        cut.top <- F
        new.title <- ""
    }, blank2 = {
        cut.top <- T
        new.title <- ""
    }, stop("Unrecognized probplot title"))
    if (cut.top) {
        title.line <- 4.1
        top.mar <- 6
        lside.line <- 4
    }
    else {
        title.line <- 0
        top.mar <- 3
        lside.line <- 4
    }
    return(list(title.line = title.line, top.mar = top.mar, lside.line = lside.line, 
        new.title = new.title))
}
