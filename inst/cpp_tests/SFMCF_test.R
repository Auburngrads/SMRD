times <- 1:10
theta <- c(2,4)
form <- 'loglinear'
old <- SMRD:::NHPP.MCFvec(times, theta, form)

time = as.double(times)
kform = as.integer(SMRD:::num.nhpp.form(SMRD:::generic.nhpp.form(form)[[1]]))
theta = as.double(theta)
ntimes = as.integer(length(times))
answer = double(length(times))

new <- SMRD2::SFMCF(time, kform, theta,ntimes,answer)
