ui = navbarPage(title = 'GOMA Distribution',
                collapsible = T, 
                position = 'fixed-top',
                theme  = SMRD::add_theme(getShinyOption('theme')),
                header = SMRD::add_css(),
                footer = SMRD::add_logo(),

tabPanel('Shiny App',
         SMRD::add_ui('distribution_goma_functions')),

tabPanel('Distribution Functions',
         uiOutput('gomafunc', class = 'ta-text')),

tabPanel('Distribution Properties',
         uiOutput('gomaprops', class = 'ta-text')))
