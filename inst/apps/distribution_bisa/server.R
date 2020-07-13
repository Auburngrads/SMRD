server = function(input, output, session) {
  
     SMRD::add_server('distribution_bisa_functions', env = environment())

output$bisafunc  <- renderUI({ SMRD::add_rmd('bisa-func.Rmd') })

output$bisaprops <- renderUI({ SMRD::add_rmd('bisa-props.Rmd') })
}