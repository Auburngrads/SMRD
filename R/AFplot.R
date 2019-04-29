AFplot <-
function (stress, stress0, coef, relationship, power, number.points = 100,
    grid = T, my.title = NULL, title.option = "full")
{
    line.map <- (1:(length(coef) + 1))[-2]
    save.coef <- coef
    if (generic.relationship.name(relationship) == "Arrhenius") {
        coef <- -coef
        relationship <- "Arrhenius3"
  } else {
        relationship <- relationship
        coef <- coef
  }

    AF <- function (stress, stress0, coef, relationship, power)
      {
        relationship <- set.relationship.power(relationship, power)
        answer <- exp(coef * (f.relationship(stress0, relationship) -
                                f.relationship(stress, relationship)))
        names(answer) <- NULL
        return(answer)
      }

    relationship <- set.relationship.power(relationship, power)
    if (length(stress) != 1 || length(stress0) != 1)
        stop("Accelerating variable values should be scalar")
    afmat <- matrix(NA, ncol = length(coef), nrow = number.points)
    stress.vec <- seq(stress0, stress, length = number.points)
    for (i in 1:length(coef)) {
        afmat[, i] <- AF(stress.vec, stress0, coef[i], relationship,
            power)
    }
    if (generic.relationship.name(relationship) == "Arrhenius3") {
        stress.units <- "Degrees C"
}   else {
        stress.units <- "Stress"
    }
    plot.paper(range(stress.vec), range(afmat), grids = grid,
        x.axis = relationship, y.axis = "log", ylab = paste("Acceleration Factor Relative to",
            stress0, stress.units), xlab = stress.units)
    for (i in 1:ncol(afmat)) {
        lines(f.relationship(stress.vec, relationship), logb(afmat[,
            i]), lwd = 3, lty = line.map[i], col = i)
    }
    if (is.null(my.title)) {
        Relat.String <- paste(relationship, "relationship")
        my.title <- switch(generic.relationship.name(relationship),
            `Box-Cox` = {
                paste(Relat.String, "with power ", power, sep = "")
            }, linear = , squareroot = , log = {
                Relat.String
            }, Arrhenius3 = , Arrhenius = {
                paste("Arrhenius relationship with activation energy ",
                  "in units of ", GetSMRDDefault("SMRD.Boltzmann"),
                  sep = "")
            })
    }
    title(my.title)
    bty = "o"
    bg0 = 16
    legend(x.loc(0.02), y.loc(0.98), as.character(save.coef),
        bty = bty, bg = bg0, lty = line.map, col = 1:length(coef),
        lwd = 3, y.intersp = 0.675)
    invisible()
}
