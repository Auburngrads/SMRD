wqmpoints <-  function(q,p,prob,sd,lsd,m) {
  
  yplot = double(m + 1)
  pplot = double(m + 1) 
  sdplot = double(m + 1) 
  mplot = integer(1)
  
      for(j in 1:m) {
        
        if(prob[j] <= 0) j = j+1
      
      sdnow = 0 
      
# do not plot anything if prob=0
         
      pnow = prob[j]
      
      if(lsd) sdnow = sd[j]

# if j=1,  then we have left censored stuff and want to plot the prob

         if(j == 1) {
          if(pnow <= 0 || pnow >= 1) j = j+1 
         }
         delta = 1
         denom = (p[j]-p[j-1])
         
         if(denom <= 0) {
           
         pnow=delta*prob[j-1]+(1-delta)*prob[j]
         sdnow=delta*sd[j-1]+(1-delta)*sd[j]
         
         }
         delta=((q[j-1]-p[j-1])/(denom))/2
         
# otherwise, do an averaging  -- first find delta
         
#check for plotting probability outside 0-1 interval
#everything ok for current point, add to the vector
         mplot=mplot+1
         yplot[mplot]=p[j]
         pplot[mplot]=pnow

         #save current values for possible use in next jump point

                  sdplot[mplot]=sdnow
         plast=prob[j]
         if(lsd == 1) sdlast=sd[j]
      }      
         out <- list()
         out$yplot  <- yplot
         out$pplot  <- pplot
         out$mplot  <- mplot
         out$sdplot <- sdplot
         
         invisible(out)
}