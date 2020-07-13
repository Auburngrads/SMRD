server = function(input, output, session) {
  
     SMRD::add_server('distribution_goma_functions', env = environment())

output$gomafunc  <- renderUI({ SMRD::add_rmd('goma-func.Rmd') })

output$gomaprops <- renderUI({ SMRD::add_rmd('goma-props.Rmd') })
}