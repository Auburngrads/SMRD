wqm.assign <-
function (x, value, frame, pos = 1, where = 1, inherits = FALSE,
    immediate = TRUE, meta = 0)
{

  if(missing(frame)) {
      
      assign(x, value, pos = pos)
      
  } else {
    
    switch(as.character(frame + 1), 
           `1` = { the.frame <- frame0 }, 
           `2` = { the.frame <- frame1 }, 
                 { cat("Illegal frame argument in wqm.assign = ", frame)
            })
            assign(x, value, pos = the.frame)
        }
    
}
