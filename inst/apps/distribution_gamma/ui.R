ui = navbarPage(title = 'Gamma Distribution',
                collapsible = T, 
                position = 'fixed-top',
                theme  = SMRD::add_theme(getShinyOption('theme')),
                header = SMRD::add_css(),
                footer = SMRD::add_logo(),


tabPanel('Shiny App',
         SMRD::add_ui('distribution_gamma_functions')),

tabPanel('Distribution Functions',
         uiOutput('gammafunc', class = 'ta-text')),

tabPanel('Distribution Properties',
         uiOutput('gammaprops', class = 'ta-text')))
