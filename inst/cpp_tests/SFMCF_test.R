times <- 1:10
theta <- c(2,4)
form <- 'loglinear'
old <- smrdfortran:::NHPP.MCFvec(times, theta, form)

time = as.double(times)
kform = as.integer(smrdfortran:::num.nhpp.form(smrdfortran:::generic.nhpp.form(form)[[1]]))
theta = as.double(theta)
ntimes = as.integer(length(times))
answer = double(length(times))

new <- wqmmlesss ::sfmcf(time, kform, theta,ntimes,answer)
