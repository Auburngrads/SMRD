FieldParameters <-
function (use.rate.model) 
{
    use.rate.parameters <- use.rate.model$use.rate.parameters
    lab.parameters <- use.rate.model$lab.parameters
    Field.muWear <- lab.parameters["Wear.lab.mu.mu"] - use.rate.parameters["Wear.mu.use.rate"]
    Field.muCrack <- lab.parameters["Crack.lab.mu.mu"] - use.rate.parameters["Crack.mu.use.rate"]
    Field.sigmaWear <- sqrt(use.rate.parameters["Wear.sigma.use.rate"]^2 + 
        lab.parameters["Wear.lab.sigma.sigma"]^2)
    Field.sigmaCrack <- sqrt(use.rate.parameters["Crack.sigma.use.rate"]^2 + 
        lab.parameters["Crack.lab.sigma.sigma"]^2)
    rhoTT <- (use.rate.model$rho * use.rate.parameters["Wear.sigma.use.rate"] * 
        use.rate.parameters["Crack.sigma.use.rate"])/(Field.sigmaWear * 
        Field.sigmaCrack)
    return(list(Field.muWear = Field.muWear, Field.muCrack = Field.muCrack, 
        Field.sigmaWear = Field.sigmaWear, Field.sigmaCrack = Field.sigmaCrack, 
        rhoTT = rhoTT))
}
