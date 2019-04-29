library(smrdfortran)
tvec = c(1:10)
r1log = .5
mut2 = 4
sigmat2 = .4
mur1 = 5
sigmar1 = .22 
mur2 = 5
sigmar2 = .65 
rho  = .5
kprint = 0

    zout <- .Fortran("ssft2gr1", 
                     tvec = as.double(tvec), 
                     nt = as.integer(length(tvec)), 
                     mut1 = as.double(r1log), 
                     mut2 = as.double(mut2), 
                     sigmat2 = as.double(sigmat2), 
                     mur1 = as.double(mur1), 
                     sigmar1 = as.double(sigmar1), 
                     mur2 = as.double(mur2), 
                     sigmar2 = as.double(sigmar2), 
                     rho = as.double(rho), 
                     answer = double(length(tvec)),
                     ier = integer(1),
                     kprint = kprint)

    new <- wqmmlesss::ssft2gr1(t = as.double(tvec), 
                     nt = as.integer(length(tvec)), 
                     as.double(r1log), 
                     mut2 = as.double(mut2), 
                     sigmat2 = as.double(sigmat2), 
                     mur1 = as.double(mur1), 
                     sigmar1 = as.double(sigmar1), 
                     mur2 = as.double(mur2), 
                     sigmar2 = as.double(sigmar2), 
                     rho = as.double(rho), 
                     answer = double(length(tvec)),
                     ier = integer(1),
                     kprint = kprint)
