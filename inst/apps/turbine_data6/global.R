library(SMRD)
pacman::p_load(DT)

Turbine.ld <- frame.to.ld(turbine,
                          response.column = 1,
                          censor.column = 2,
                          case.weight.column=3,
                          time.units = "Hundred Hours")