set.arrhenius.units <-
function (relationship) 
{
    is.arrhenius.type <- is.element(multiple.generic.relationship.name(relationship), 
        c("Arrhenius", "Arrhenius3", "Eyring"))
    if (any(is.arrhenius.type)) {
        attr(relationship, "the.arrhenius.units") <- GetSMRDDefault("SMRD.Boltzmann")
    }
    return(relationship)
}
