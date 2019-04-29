FieldParameters2 <-
function (theta) 
{
    Field.sigmaCrack <- sqrt(theta["Crack.sigma.cycles"]^2 + 
        theta["Crack.sigma.use.rate"]^2)
    delta.logitFsys <- Field.sigmaWear <- sqrt(theta["Wear.sigma.cycles"]^2 + 
        theta["Wear.sigma.use.rate"]^2)
    Field.muWear <- theta["Wear.lab.mu.mu"] - theta["Wear.mu.use.rate"]
    Field.muCrack <- theta["Crack.lab.mu.mu"] - theta["Crack.mu.use.rate"]
    rhoTT <- (theta["rhoRR"] * theta["Wear.sigma.use.rate"] * 
        theta["Crack.sigma.use.rate"])/(Field.sigmaWear * Field.sigmaCrack)
    return(list(Field.muWear = Field.muWear, Field.muCrack = Field.muCrack, 
        Field.sigmaWear = Field.sigmaWear, Field.sigmaCrack = Field.sigmaCrack, 
        rhoTT = rhoTT))
}
