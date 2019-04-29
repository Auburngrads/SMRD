RecipBoltzmann <-
function (the.units, printem = T) 
{
    the.names <- c("eV", "kJ/mole", "kcal/mole")
    the.numbers <- c(11604.83, 120.279, 503.2206)
    names(the.numbers) <- the.names
    if (missing(the.units)) {
        if (printem) 
            print(the.numbers, digits = 10)
        return(the.numbers)
    }
    else {
        the.recip <- the.numbers[the.units]
        if (is.na(the.recip)) 
            stop(paste(the.units, "is not a recognized Boltzmann/gas constant units"))
        return(the.recip)
    }
}
