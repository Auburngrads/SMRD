generic.relationship.name <-
function (relationship, allow = F) 
{
    switch(as.character(relationship), invtemp = , temp = {
        return("invtemp")
    }, invtemp3 = , temp = {
        return("invtemp3")
    }, Arrhenius = , arrhenius = , Arrhenius2 = , arrhenius2 = {
        return("Arrhenius")
    }, Eyring = , eyring = {
        return("Eyring")
    }, `Box-Cox` = , `box-cox` = , Power = , power = {
        return("Box-Cox")
    }, Arrhenius3 = , arrhenius3 = {
        return("Arrhenius3")
    }, `Power Rule` = , InversePowerRule = , `Inverse Power Rule` = , 
        `Inverse power rule` = , `inverse power rule` = , Log = , 
        log = {
            return("log")
        }, `Power Rule10` = , `Inverse Power Rule10` = , `Inverse power rule10` = , 
        `inverse power rule10` = , Log10 = , log10 = {
            return("log10")
        }, Squareroot = , SquareRoot = , `Square root` = , `Square Root` = , 
        sqrt = , squareroot = {
            return("squareroot")
        }, Linear = , linear = {
            return("linear")
        }, Reciprocal = , reciprocal = , Inverse = , inverse = {
            return("reciprocal")
        }, Reciprocal3 = , reciprocal3 = , Inverse3 = , inverse3 = {
            return("reciprocal3")
        }, Humidity = , humidity = {
            return("humidity")
        }, Logit = , logit = {
            return("logit")
        }, logit2 = {
            return("logit2")
        }, Class = , class = {
            return("class")
        }, blank = , zero = {
            return("linear")
        }, if (!allow) stop(paste(relationship, " is an unrecognized relationship")))
    return(relationship)
}
