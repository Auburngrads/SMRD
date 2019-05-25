library(SMRD)
library(SMRD2)
test = 1

if(test == 1){
  
ndist1 = 1
ndist2 = 3
beta0 = 30.27241
beta1 = -5.100121
stress = 270
sigma = 0.2894549
ugamma = 5.365834
sgamma = 0.03140004
w = logb(5000)

}

if(test == 2){
ndist1 = 1
ndist2 = 2
beta0 = 9.3562771160
beta1 = -2.9350357450 
sigma = 0.0002000000 
ugamma = 3.8630272310
sgamma = 0.1183292607
stress = c(58.00, 60.00, 50.00, 57.75, 66.13, 48.04, 
            55.16, 51.54, 62.76, 47.50, 48.00, 55.58) 
w = c(16.11809565, 16.11809565, 20.72326584, 20.72326584,
       11.75529108, 12.20936721, 16.11809565, 11.66641021,
       11.83384887, 16.11809565, 16.11809565, 12.21484406) 
}
if(test == 3){
j = 1
ndist1 = 1
ndist2 = 1
beta0 = 9.3562771160
beta1 = -2.9350357450 
sigma = 0.0002000000 
ugamma = 3.8630272310
sgamma = 0.1183292607
stress = c(58.00, 60.00, 50.00, 57.75, 66.13, 48.04, 
            55.16, 51.54, 62.76, 47.50, 48.00, 55.58) 
stress = stress[j]
w = c(16.11809565, 16.11809565, 20.72326584, 20.72326584,
       11.75529108, 12.20936721, 16.11809565, 11.66641021,
       11.83384887, 16.11809565, 16.11809565, 12.21484406) 
w = w[j]
}

debug1 = F
if(!exists("kprint")) kprint = 0

      max.length <- max(length(beta0), length(beta1), length(sigma),
                        length(ugamma), length(sgamma), length(stress), length(w))
      beta0  <- SMRD2:::expand.vec(beta0, max.length)
      beta1  <- SMRD2:::expand.vec(beta1, max.length)
      sigma  <- SMRD2:::expand.vec(sigma, max.length)
      ugamma <- SMRD2:::expand.vec(ugamma, max.length)
      sgamma <- SMRD2:::expand.vec(sgamma, max.length)
      stress <- SMRD2:::expand.vec(stress, max.length)
      w      <- SMRD2:::expand.vec(w, max.length)
      if (debug1) browser()
      zout <- .Fortran("sxpdf3", as.integer(ndist1), as.integer(ndist2),
                       as.double(beta0), as.double(beta1), as.double(stress),
                       as.double(sigma), as.double(ugamma), as.double(sgamma),
                       as.double(w), as.integer(max.length), answer = double(max.length),
                       ier = integer(max.length))
      
      new = SMRD2:::SXPDF3(as.integer(ndist1),
                           as.integer(ndist2),
                           as.double(beta0),
                           as.double(beta1),
                           as.double(stress),
                           as.double(sigma),
                           as.double(ugamma),
                           as.double(sgamma),
                           as.double(w),
                           as.integer(max.length),
                           answer = double(max.length),
                           ier = integer(max.length),
                           as.integer(kprint))
