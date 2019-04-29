SMRDObjectDefineName <-
  function ()
  {
    the.type.options <- c("Probability of successful demonstration simulation objects",
                          "ALT plan objects", "Plan value objects", "ALT plan value objects",
                          "Life data objects", "Recurrence data objects", "Repeated measures degradation data objects",
                          "Regression fitting results objects", "Single distribution (mlest) results objects",
                          "Likelihood simulation objects", "Bootstrap results objects",
                          "Prior distribution specification objects", "Posterior distribution objects",
                          "Special distribution (gmle) results objects", "ALT plan simulation objects",
                          "Destructive degradation data objects", "Destructive degradation results objects",
                          "Destructive degradation plan value objects", "Destructive degradation test plan objects",
                          "Destructive degradation test plan simulation output",
                          "Repeated measures degradation test plan simulation output")

    the.code.names <- c("SMRDXXXprsdObjects", "SMRD.alt.test.plan.objects",
                        "SMRDPvObjects", "SMRD.alt.plan.values.objects",
                        "SMRDXXXLdObjects", "SMRDRdObjects", "SMRDRmdObjects",
                        "SMRD.alt.regr.output.objects", "SMRD.mlest.output.objects",
                        "SMRD.likelihood.region.objects", "SMRD.parametric.bootstrap.objects",
                        "SMRD.prior.spec.objects", "SMRD.posterior.objects",
                        "SMRD.gmle.out.objects", "SMRD.ALT.Simulation.output",
                        "SMRDDddObjects", "SMRD.Ddd.output.objects", "SMRD.ADDT.plan.values",
                        "SMRD.ADDT.test.plan.objects", "SMRD.simulate.ADDT.out",
                        "SMRD.simulate.RMD.out")
    if (length(the.code.names) != length(the.type.options))
      warning(paste("Length of the names is different than strings",
                        paste("names= ", length(the.code.names), ", strings= ",
                              length(the.type.options), collapse = "")))
    names(the.code.names) <- the.type.options
    return(the.code.names)
  }
