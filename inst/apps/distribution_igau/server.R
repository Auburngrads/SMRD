server = function(input, output, session) {
  
     SMRD::add_server('distribution_igau_functions', env = environment())

output$igaufunc  <- renderUI({ SMRD::add_rmd('igau-func.Rmd') })

output$igauprops <- renderUI({ SMRD::add_rmd('igau-props.Rmd') })
}