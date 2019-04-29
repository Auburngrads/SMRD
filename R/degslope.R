degslope <-
function (beta1, beta2, AccVar, transformation.x = "Arrhenius3") 
{
    transformation.x <- set.relationship.power(transformation.x, 
        NULL)
    beta1 * exp(beta2 * f.relationship(AccVar, transformation.x))
}
