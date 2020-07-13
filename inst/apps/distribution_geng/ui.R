ui = navbarPage(title = 'Generalized Gamma Distribution',
                collapsible = T, 
                position = 'fixed-top',
                theme  = SMRD::add_theme(getShinyOption('theme')),
                header = SMRD::add_css(),
                footer = SMRD::add_logo(),


tabPanel('Shiny App',
         SMRD::add_ui('distribution_geng_functions')),

tabPanel('Distribution Functions',
         uiOutput('gengfunc', class = 'ta-text')),

tabPanel('Distribution Properties',
         uiOutput('gengprops', class = 'ta-text')))
