fix.inverse.relationship <-
function (relationship) 
{
    for (i in 1:length(relationship)) {
        switch(generic.relationship.name(relationship[i]), Arrhenius = {
            relationship[i] <- "Arrhenius3"
            slope.name <- "   AvHW(Ea)="
        }, reciprocal = {
            relationship[i] <- "reciprocal3"
            slope.name <- " AvHW(slope)="
        }, invtemp = {
            relationship[i] <- "invtemp3"
            slope.name <- " AvHW(slope)="
        }, {
            slope.name <- " AvHW(slope)="
        })
    }
    attr(relationship, "slope.name") <- slope.name
    return(relationship)
}
