profile.list <-
function (fitted, imid = fitted + 1,...)
return(c(imid:1, imid:(2 * fitted + 1)))
