switch.units <- 
function(units,data.d = NULL) 
{
 
if(!is.null(data.d)) {
  
  `if`(is.onlist(class(data.d)[[1]], "multiple.life.data"),
       unit.names <- as.vector(attr(data.d[[1]],"x.columns")),
       unit.names <- as.vector(attr(data.d,"x.columns")))
   
  if(is.onlist(class(data.d)[[1]], "multiple.failure.mode.life.data")){
    
     unit.names <- names(data.d)
     
  }
  
  for(i in 1:length(unit.names)){
    
      units <- gsub(unit.names[i], paste0("~", unit.names[i]), units)
    
  }
  
}
  
  units <- tolower(units)
  
  units <- gsub("(\\d*)([.]\\s*|\\s*)(celsius|degreesc|temp)", "\\1~degree*C", units)
  units <- gsub("(\\d*)([.]\\s*|\\s*)(volts)", "\\1~V",      units)
  units <- gsub("(\\d*)([.]\\s*|\\s*)(farenheit|degreesf)", "\\1~degree*F", units)
  units <- gsub("(\\d*)([.]\\s*|\\s*)(kelvin)","\\1~K",      units)
  units <- gsub("(\\d*)([.]\\s*|\\s*)(rankine)","\\1~R",     units)
  units <- gsub("(\\d*)([.]\\s*|\\s*)(rh)","\\1~RH",     units)
  units <- gsub(";", "~~",         units)
  units <- gsub("([[:punct:]]|[[:space:]])per([[:punct:]]|[[:space:]])", "/",units)
  
  invisible(units)
}
  