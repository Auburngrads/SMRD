#' Unicode Greek letters
#'
#' @param ltr A english language representation 
#'            of a letter from the Greek alphabet
#'            
#' @return A Unicode representation of a letter
#'         from the Greek alphabet

greek <- function(ltr){

if(ltr == "Alpha")   return("\u0391")
if(ltr == "Beta")    return("\u0392")
if(ltr == "Gamma")   return("\u0393")
if(ltr == "Delta")   return("\u0394")
if(ltr == "Epsilon") return("\u0395")
if(ltr == "Zeta")    return("\u0396")
if(ltr == "Eta")     return("\u0397")
if(ltr == "Theta")   return("\u0398")
if(ltr == "Iota")    return("\u0399")
if(ltr == "Kappa")   return("\u039A")
if(ltr == "Lambda")  return("\u039B")
if(ltr == "Mu")      return("\u039C")
if(ltr == "Nu")      return("\u039D")
if(ltr == "Xi")      return("\u039E")
if(ltr == "Omicron") return("\u039F")
if(ltr == "Pi")      return("\u03A0")
if(ltr == "Rho")     return("\u03A1")
if(ltr == "Sigma")   return("\u03A3")
if(ltr == "Tau")     return("\u03A4")
if(ltr == "Upsilon") return("\u03A5")
if(ltr == "Phi")     return("\u03A6")
if(ltr == "Chi")     return("\u03A7")
if(ltr == "Psi")     return("\u03A8")
if(ltr == "Omega")   return("\u03A9")
if(ltr == "alpha")   return("\u03B1")
if(ltr == "beta")    return("\u03B2")
if(ltr == "gamma")   return("\u03B3")
if(ltr == "delta")   return("\u03B4")
if(ltr == "epsilon") return("\u03B5")
if(ltr == "zeta")    return("\u03B6")
if(ltr == "eta")     return("\u03B7")
if(ltr == "theta")   return("\u03B8")
if(ltr == "iota")    return("\u03B9")
if(ltr == "kappa")   return("\u03BA")
if(ltr == "lambda")  return("\u03BB")
if(ltr == "mu")      return("\u03BC")
if(ltr == "nu")      return("\u03BD")
if(ltr == "xi")      return("\u03BE")
if(ltr == "omicron") return("\u03BF")
if(ltr == "pi")      return("\u03C0")
if(ltr == "rho")     return("\u03C1")
if(ltr == "sigma")   return("\u03C3")
if(ltr == "tau")     return("\u03C4")
if(ltr == "upsilon") return("\u03C5")
if(ltr == "phi")     return("\u03C6")
if(ltr == "chi")     return("\u03C7")
if(ltr == "psi")     return("\u03C8")
if(ltr == "omega")   return("\u03C9")
  
  return("letter not found - check your spelling")

}