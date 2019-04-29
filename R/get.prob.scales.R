get.prob.scales <-
function (distribution, ylab = GetSMRDDefault("SMRD.LabelOnYaxis"), 
    prob.range = NULL, shape = NULL) 
{
    sev.tick.location <- c(".00000001", ".00000002", ".00000003", 
        ".00000005", ".0000001", ".0000002", ".0000003", ".0000005", 
        ".000001", ".000002", ".000003", ".000005", ".00001", 
        ".000012", ".000014", ".000016", ".000018", ".00002", 
        ".000025", ".00003", ".000035", ".00004", ".000045", 
        ".00005", ".00006", ".00007", ".00008", ".00009", ".0001", 
        ".00012", ".00014", ".00016", ".00018", ".0002", ".00025", 
        ".0003", ".0004", ".00045", ".0005", ".0006", ".0007", 
        ".0008", ".0009", ".001", ".0012", ".0014", ".0016", 
        ".0018", ".002", ".0025", ".003", ".0035", ".004", ".0045", 
        ".005", ".006", ".007", ".008", ".009", ".01", ".012", 
        ".014", ".016", ".018", ".02", ".022", ".024", ".026", 
        ".028", ".03", ".035", ".04", ".045", ".05", ".06", ".07", 
        ".08", ".09", ".1", ".12", ".14", ".16", ".18", ".2", 
        ".22", ".24", ".26", ".28", ".3", ".35", ".4", ".45", 
        ".5", ".55", ".6", ".65", ".7", ".75", ".8", ".85", ".9", 
        ".92", ".94", ".96", ".98", ".99", ".995", ".999", ".9999", 
        ".99999", ".999999", ".9999999")
    sev.tick.labels <- c(".00000001", ".00000002", ".00000003", 
        ".00000005", ".0000001", ".0000002", ".0000003", ".0000005", 
        ".000001", ".000002", ".000003", ".000005", ".00001", 
        ".00002", ".00003", ".00005", ".0001", ".0002", ".0003", 
        ".0005", ".001", ".003", ".005", ".01", ".02", ".03", 
        ".05", ".1", ".2", ".3", ".5", ".7", ".9", ".98", ".99", 
        ".999", ".9999", ".99999", ".999999", ".9999999")
    sev.Percent.tick.labels <- c(".000001", ".000002", ".000003", 
        ".000005", ".00001", ".00002", ".00003", ".00005", ".0001", 
        ".0002", ".0003", ".0005", ".001", ".002", ".003", ".005", 
        ".01", ".02", ".03", ".05", ".1", ".3", ".5", "1", "2", 
        "3", "5", "10", "20", "30", "50", "70", "90", "98", "99", 
        "99.9", "99.99", "99.999", "99.9999", "99.99999")
    normal.tick.location <- c(".00000001", ".00000002", ".00000003", 
        ".00000005", ".0000001", ".0000002", ".0000003", ".0000005", 
        ".000001", ".000002", ".000003", ".000005", ".00001", 
        ".000012", ".000014", ".000016", ".000018", ".00002", 
        ".000025", ".00003", ".000035", ".00004", ".000045", 
        ".00005", ".00006", ".00007", ".00008", ".00009", ".0001", 
        ".0002", ".0003", ".0004", ".0005", ".0006", ".0007", 
        ".0008", ".0009", ".001", ".0015", ".002", ".003", ".004", 
        ".005", ".0075", ".01", ".015", ".02", ".03", ".04", 
        ".05", ".06", ".07", ".08", ".09", ".1", ".12", ".14", 
        ".16", ".18", ".2", ".25", ".3", ".35", ".4", ".45", 
        ".5", ".55", ".6", ".65", ".7", ".75", ".8", ".82", ".84", 
        ".86", ".88", ".9", ".91", ".92", ".93", ".94", ".95", 
        ".96", ".97", ".98", ".985", ".99", ".9925", ".995", 
        ".996", ".997", ".998", ".9985", ".999", ".9991", ".9992", 
        ".9993", ".9994", ".9995", ".9996", ".9997", ".9998", 
        ".9999", ".99999", ".999999", ".9999999")
    normal.tick.labels <- c(".00000001", ".00000002", ".00000003", 
        ".00000005", ".0000001", ".0000002", ".0000003", ".0000005", 
        ".000001", ".000002", ".000003", ".000005", ".00001", 
        ".00005", ".0001", ".0005", ".001", ".002", ".005", ".01", 
        ".02", ".05", ".1", ".2", ".3", ".4", ".5", ".6", ".7", 
        ".8", ".9", ".95", ".98", ".99", ".995", ".998", ".999", 
        ".9995", ".9999", ".99999", ".999999", ".9999999")
    normal.Percent.tick.labels <- c(".000001", ".000002", ".000003", 
        ".000005", ".00001", ".00002", ".00003", ".00005", ".0001", 
        ".0002", ".0003", ".0005", ".001", ".005", ".01", ".05", 
        ".1", ".2", ".5", "1", "2", "5", "10", "20", "30", "40", 
        "50", "60", "70", "80", "90", "95", "98", "99", "99.5", 
        "99.8", "99.9", "99.95", "99.99", "99.999", "99.9999", 
        "99.99999")
    exponential.tick.location <- c(".001", ".005", ".01", "0.05", 
        ".1", ".15", ".2", ".25", ".30", ".35", ".4", ".45", 
        ".4", ".5", ".55", ".6", ".45", ".65", ".7", ".75", ".8", 
        ".85", ".9", ".91", ".92", ".93", ".94", ".95", ".96", 
        ".97", ".98", ".985", ".991", ".992", ".993", ".994", 
        ".995", ".996", ".997", ".998", ".9985", ".999", ".9991", 
        ".9992", ".9993", ".9994", ".9995", ".9997", ".9998", 
        ".9999", ".99999", ".9999999", ".9999999")
    exponential.tick.labels <- c(".001", ".01", ".1", ".2", ".3", 
        ".4", ".5", ".6", ".7", ".8", ".9", ".95", ".98", ".99", 
        ".995", ".998", ".999", ".9995", ".9998", ".9999", ".99999", 
        ".999999", ".9999999")
    exponential.Percent.tick.labels <- c(".1", "1", "10", "20", 
        "30", "40", "50", "60", "70", "80", "90", "95", "98", 
        "99", "99.5", "99.8", "99.9", "99.95", "99.98", "99.99", 
        "99.999", "99.9999", "99.99999")
    uniform.tick.location <- c("0", ".01", ".02", ".03", ".04", 
        ".05", ".06", ".07", ".08", ".09", ".1", ".11", ".12", 
        ".13", ".14", ".15", ".16", ".17", ".18", ".19", ".2", 
        ".21", ".22", ".23", ".24", ".25", ".26", ".27", ".28", 
        ".29", ".3", ".31", ".32", ".33", ".34", ".35", ".36", 
        ".37", ".38", ".39", ".4", ".41", ".42", ".43", ".44", 
        ".45", ".46", ".47", ".48", ".49", ".5", ".51", ".52", 
        ".53", ".54", ".55", ".56", ".57", ".58", ".59", ".6", 
        ".61", ".62", ".63", ".64", ".65", ".66", ".67", ".68", 
        ".69", ".7", ".71", ".72", ".73", ".74", ".75", ".76", 
        ".77", ".78", ".79", ".8", ".81", ".82", ".83", ".84", 
        ".85", ".86", ".87", ".88", ".89", ".9", ".91", ".92", 
        ".93", ".94", ".95", ".96", ".97", ".98", ".99", "1.")
    uniform.tick.labels <- c("0", ".1", ".2", ".3", ".4", ".5", 
        ".6", ".7", ".8", ".9", "1.")
    uniform.Percent.tick.labels <- c("0", "10", "20", "30", "40", 
        "50", "60", "70", "80", "90", "100")
    distribution <- generic.distribution(distribution)
    if (is.null(prob.range)) 
        prob.range <- range(as.numeric(sev.tick.labels))
    dist.info.out <- dist.info(distribution)
    if (length(shape) < dist.info.out$num.shape.needed) 
        stop(paste("Need", dist.info.out$num.shape.needed, "shape parameters for", 
            distribution, "distribution--", length(shape), "provided"))
    if (is.null(shape)) {
        switch(distribution, gng = {
            tick.labels <- sev.tick.labels
            tick.location <- sev.tick.location
            percent.tick.labels <- sev.Percent.tick.labels
        }, weibull = , sev = {
            tick.labels <- sev.tick.labels
            tick.location <- sev.tick.location
            percent.tick.labels <- sev.Percent.tick.labels
        }, loglogistic = , logistic = {
            tick.labels <- normal.tick.labels
            tick.location <- normal.tick.location
            percent.tick.labels <- normal.Percent.tick.labels
        }, lognormal = , normal = {
            tick.labels <- normal.tick.labels
            tick.location <- normal.tick.location
            percent.tick.labels <- normal.Percent.tick.labels
        }, frechet = , lev = {
            tick.labels <- normal.tick.labels
            tick.location <- normal.tick.location
            percent.tick.labels <- normal.Percent.tick.labels
        }, uniform = {
            tick.labels <- wqm.pretty(prob.range, nint = 5)
            tick.location <- wqm.pretty(tick.labels, nint = 25)
            percent.tick.labels <- tick.labels * 100
        }, loguniform = {
            tick.labels <- wqm.pretty(prob.range, nint = 5)
            tick.location <- wqm.pretty(tick.labels, nint = 25)
            percent.tick.labels <- wqm.pretty(prob.range, nint = 5) * 
                100
        }, exponential = {
            if (prob.range[2] > 0.01) {
                tick.labels <- exponential.tick.labels
                tick.location <- exponential.tick.location
                percent.tick.labels <- exponential.Percent.tick.labels
            } else {
                tick.labels <- sev.tick.labels
                tick.location <- sev.tick.location
                percent.tick.labels <- sev.Percent.tick.labels
            }
        }, stop("Distribution not recognized"))
        if (dist.info.out$take.logs == "never") 
            logger <- F
        else logger <- T
  } else {
        if (!is.logdist(distribution)) 
            stop(paste("Shape parameter provided for", distribution, 
                "distribution"))
        if (prob.range[2] > 0.01) {
            tick.location <- exponential.tick.location
            tick.labels <- exponential.tick.labels
            percent.tick.labels <- as.character(as.numeric(tick.labels) * 
                100)
      } else {
            tick.location <- sev.tick.location
            tick.labels <- sev.tick.labels
            percent.tick.labels <- as.character(as.numeric(tick.labels) * 
                100)
        }
        if (dist.info.out$take.logs == "always") 
            logger <- T
        else logger <- F
    }
    return(list(tick.location = tick.location, tick.labels = tick.labels, 
        percent.tick.labels = percent.tick.labels, logger = logger, 
        distribution = dist.info.out$formal.name, prob.scale = dist.info.out$prob.scale))
}
