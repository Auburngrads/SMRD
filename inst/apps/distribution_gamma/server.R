server = function(input, output, session) {
  
     SMRD::add_server('distribution_gamma_functions', env = environment())

output$gammafunc  <- renderUI({ SMRD::add_rmd('gamma-func.Rmd') })

output$gammaprops <- renderUI({ SMRD::add_rmd('gamma-props.Rmd') })
}