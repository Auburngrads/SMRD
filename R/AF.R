AF <-
function (stress, stress0, coef, relationship, power)
{
    relationship <- set.relationship.power(relationship, power)
    answer <- exp(coef * (f.relationship(stress0, relationship) -
        f.relationship(stress, relationship)))
    names(answer) <- NULL
    return(answer)
}
