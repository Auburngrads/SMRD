ui = navbarPage(title = 'BISA Distribution',
                collapsible = T, 
                position = 'fixed-top',
                theme  = SMRD::add_theme(getShinyOption('theme')),
                header = SMRD::add_css(),
                footer = SMRD::add_logo(),

tabPanel('Shiny App',
         SMRD::add_ui('distribution_bisa_functions')),

tabPanel('Distribution Functions',
         uiOutput('bisafunc', class = 'ta-text')),

tabPanel('Distribution Properties',
         uiOutput('bisaprops', class = 'ta-text')))
