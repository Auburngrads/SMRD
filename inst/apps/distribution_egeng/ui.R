ui = navbarPage(title = 'Extended Generalized Gamma Distribution',
                collapsible = T, 
                position = 'fixed-top',
                theme  = SMRD::add_theme(getShinyOption('theme')),
                header = SMRD::add_css(),
                footer = SMRD::add_logo(),


tabPanel('Shiny App',
         SMRD::add_ui('distribution_egeng_functions')),

tabPanel('Distribution Functions',
         uiOutput('egengfunc', class = 'ta-text')),

tabPanel('Distribution Properties',
         uiOutput('egengprops', class = 'ta-text')))
