#' @export
plot.groupm.out <-
function (x,
          focus.variable,
          fixed.other.values, 
          range.of.focus = range(xmat(data.ld)[[focus.variable]]),
          ylim = c(NA, NA), 
          xlim = c(NA, NA),
          xlab = NULL, 
          ylab = NULL,
          grids = F,
          title.option = GetSMRDDefault("SMRD.TitleOption"), 
          response.on.yaxis = T,
          dummy.for.fixed = F, 
          point.from.xmat = NULL, 
          density.at = "Automatic",
          censor.time = NULL, 
          quant.lines = c(0.1, 0.5, 0.9), 
          add = F,
          plot.quant.labels = T, 
          my.title = NULL, 
          include.data = F,...)
{
  
    `if`(!is.onlist("life.data", oldClass(x[[1]])),
         groupm.out <- x[[1]],
         groupm.out <- x)
    
    data.ld <- groupm.out$data.ld
    
    if (!missing(fixed.other.values)) {
      
        dummy.groupm.out <- get.conditional.groupm.out(focus.variable = focus.variable,
                                                       fixed.other.values = fixed.other.values, 
                                                       groupm.out = groupm.out)
  } else {
    
        if (is.null(groupm.out$focus.variable)) groupm.out$focus.variable <- focus.variable
        dummy.groupm.out <- groupm.out
        
  }
    
    plot.alt.fit(x = dummy.groupm.out, 
                 ylim = ylim, 
                 xlim = xlim,
                 xlab = xlab, 
                 ylab = ylab, 
                 grids = grids, 
                 title.option = title.option,
                 response.on.yaxis = response.on.yaxis, 
                 my.title = my.title,
                 include.data = include.data, 
                 density.at = density.at,
                 censor.time = censor.time, 
                 quant.lines = quant.lines,
                 add = add, 
                 plot.quant.labels = plot.quant.labels, 
                 range.of.focus = range.of.focus,...)
    
    invisible()
    
}
