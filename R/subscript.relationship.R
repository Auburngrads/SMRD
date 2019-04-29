subscript.relationship <-
function (relationship, i, allow = F)
{
    funny.sub <- ifelse(is.character(i), !is.element(i, names(relationship)),
        i > length(relationship))
    if (length(i) > 1 || i == 0 || funny.sub)
        stop(paste(c("illegal subscript", i, "for relationship",
            paste(relationship, collapse = ","))))
    if (i > 0) {
        return.relationship <- multiple.generic.relationship.name(relationship)[i]
        if (any(is.element(multiple.generic.relationship.name(relationship),
            c("Box-Cox", "Eyring")))) {
            if (is.element("the.power", names(attributes(relationship))))
                attr(return.relationship, "the.power") <- recover.attribute(relationship,
                  "the.power")[i]
            else if (!allow)
                stop(c("No power attribute on relationship",
                  paste(relationship, collapse = ",")))
        }
        if (any(is.element(return.relationship, c("Arrhenius",
            "Arrhenius3", "Eyring")))) {
            attr(return.relationship, "the.arrhenius.units") <- attr(relationship,
                "the.arrhenius.units")
        }
    }
    if (i < 0) {
        return.relationship <- relationship[i]
        if (any(is.element(multiple.generic.relationship.name(relationship),
            c("Box-Cox", "Eyring")))) {
            the.power <- recover.attribute(relationship, "the.power")[i]
            attr(return.relationship, "the.power") <- the.power[i]
        }
        if (any(is.element(return.relationship, c("Arrhenius",
            "Arrhenius3", "Eyring")))) {
            attr(return.relationship, "the.arrhenius.units") <- attr(relationship,
                "the.arrhenius.units")
        }
    }
    return(return.relationship)
}
