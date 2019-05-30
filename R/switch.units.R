switch.units <- 
function(units,data.d) 
{
  
  if(any(is.onlist(class(data.d), "multiple.failure.mode.life.data"))){
    
     unit.names <- names(data.d)
     
  }
  
  `if`(is.onlist(class(data.d)[[1]], "multiple.life.data"),
       unit.names <- as.vector(attr(data.d[[1]],"x.columns")),
       unit.names <- as.vector(attr(data.d,"x.columns")))
  
  for(i in 1:length(unit.names)){
    
      units <- gsub(unit.names[i], paste0("~", unit.names[i]), units)
    
  }
  
  units <- tolower(units)
  
  units <- gsub("degreesc", "C^o", units)
  units <- gsub("celsius",  "C^o", units)
  units <- gsub("volts", "V",      units)
  units <- gsub("temp", "C^o",     units)
  units <- gsub("degreesf", "F^o", units)
  units <- gsub("farenheit","F^o", units)
  units <- gsub("kelvin","K",      units)
  units <- gsub("rankine","R",     units)
  units <- gsub(";", "~~",         units)
  
  invisible(units)
}
  