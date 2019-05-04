trellis.plot.repeated.measures.data <-
function (x, 
          title.option = GetSMRDDefault("SMRD.TitleOption"), 
          relationship.response = "linear",
          relationship.time = "linear", 
          my.title = NULL, 
          outer.plot = F,
          order.groups = F, 
          xlab = NULL, 
          ylab = NULL, 
          x.columns, 
          subset = T,...)
{
  SMRDgroupedData <-
    function (formula, 
              data, 
              order.groups = TRUE,
              FUN = function(x) max(x, na.rm = TRUE), 
              outer = NULL, inner = NULL,
              labels = NULL, units = NULL)
    {
      if(!(inherits(formula, "formula") && length(formula) == 3)) {
        stop("first argument to groupedData must be a two-sided formula")
      }
      
      if(is.null(grpForm <- nlme::getGroupsFormula(formula, asList = TRUE))) {
        stop("Right hand side of first argument must be a conditional expression")
      }
      
      func.call <- match.call()
      mCall <- as.list(match.call())[-1]
      
      nfgd <- nlme::nfGroupedData
      nmgd <- nlme::nmGroupedData
      
      `if`(length(grpForm) == 1,
           func.call <- nfgd,
           func.call <- nmgd)
      
      result <- do.call(func.call, 
                        args = list(formula, data, order.groups,
                                    FUN, outer, inner, labels, units ),
                        envir = .frame0)
      return(result)
    }

    response.column <- attr(x, "response.column")
    time.column <- attr(x, "time.column")
    unit.column <- attr(x, "unit.column")
    time.units <- attr(x, "time.units")
    response.units <- get.response.units(x)
    unit.column <- attr(x, "unit.column")
    Unit.marker <- x[[unit.column]]
    subset <- get.subset.vector(subset, x)
    subset.name <- attr(subset, "subset.name")
    
    my.title <- `if`(title.option != "blank" && is.null(my.title),
                     paste(get.data.title(x), 
                           subset.name,
                           "\nx axis:", 
                           relationship.time, "    y axis:", 
                           relationship.response),
                     "")
    
    x <- x[subset, ]
    Unit.marker <- Unit.marker[subset]
    the.frame <- x
    the.formula <- paste(response.column, "~", 
                         time.column, "|",
                         unit.column)
    
    the.formula <- as.formula(the.formula)
    
    if (missing(x.columns)) x.columns <- get.x.columns(x)
    
    if (!is.null(x.columns)) {
      
        the.outer <- as.formula(paste("~", paste(x.columns, collapse = "*")))
        
        for (i in 1:length(x.columns)) {
            the.frame[[x.columns[i]]] <- factor(the.frame[[x.columns[i]]])
        }

        } else { the.outer <- NULL }
    
    response.log.mark <- ""
    
    if(tolower(relationship.response) != "linear") {
      
        response.log.mark <- relationship.response
        
        the.frame[[response.column]] <- 
          f.relationship(as.matrix(the.frame[[response.column]]),
                         relationship.response)
    }
    
    time.log.mark <- ""
    
    if(tolower(relationship.time) != "linear") {
      
        time.log.mark <- relationship.time
        the.frame[[time.column]] <- f.relationship(as.matrix(the.frame[[time.column]]),
            relationship.time)
    }
    
    the.groupedData <- SMRDgroupedData(formula = the.formula,
                                       data = the.frame, 
                                       outer = the.outer, 
                                       order.groups = order.groups)
    
    Time <- as.matrix(the.frame[[time.column]])
    the.response <- as.matrix(the.frame[[response.column]])
    
    dimnames(Time) <- list(as.character(1:nrow(Time)), "Time")
    dimnames(the.response) <- list(as.character(1:nrow(the.response)), "Response")
    relationship.sanity(Time, relationship.time)
    relationship.sanity(the.response, relationship.time)
    
    if (is.null(xlab)) xlab <- time.units
    xlab <- paste(time.log.mark, xlab)
    
    if (is.null(ylab)) ylab <- response.units
    ylab <- paste(response.log.mark, ylab)
    
    the.plot <- `if`(is.null(the.outer) || !outer.plot,
                     wqm.plot.nfnGroupedData(the.groupedData,
                                             xlab = xlab, 
                                             ylab = ylab, 
                                             main = my.title),
                     wqm.plot.nfnGroupedData(the.groupedData,
                                             outer = outer.plot, 
                                             xlab = xlab, 
                                             ylab = ylab, 
                                             main = my.title))

    print(the.plot)
    invisible(the.groupedData)
}
