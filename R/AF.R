#' Acceleration factors
#'
#' @param stress 
#' @param stress0 
#' @param coef 
#' @param relationship 
#' @param power 
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' AF(160,80,.8,"arrhenius")
#' 
#' AFplot(160,80,.8,"arrhenius")
#' 
#' AFplot(160,80,c(.7,.8,.9),"arrhenius")
#' 
#' AF(130,110,-2,"Inverse Power Rule")
#' 
#' AFplot(130,110,-2,"Inverse Power Rule")
#' }
AF <-
function (stress, stress0, coef, relationship, power)
{
    relationship <- set.relationship.power(relationship, power)
    answer <- exp(coef * (f.relationship(stress0, relationship) -
        f.relationship(stress, relationship)))
    names(answer) <- NULL
    return(answer)
}
