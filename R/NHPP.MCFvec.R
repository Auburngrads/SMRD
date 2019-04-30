NHPP.MCFvec <-
function (times, 
          theta, 
          form)
{
    zout <- SFMCF(time = as.double(times), 
                  kform = as.integer(num.nhpp.form(generic.nhpp.form(form)[[1]])),
                  theta = as.double(theta), 
                  ntimes = as.integer(length(times)),
                  answer = double(length(times)))
    
    return(zout$answer)
    
}
