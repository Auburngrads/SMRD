ui = navbarPage(title = 'Example 1.8',
                collapsible = T, 
                position = 'fixed-top',
                theme  = SMRD::add_theme(getShinyOption('theme')),
                header = SMRD::add_css(),
                footer = SMRD::add_logo(),


tabPanel('Background',
         mainPanel(uiOutput('example1.8'), class = 'shiny-text-output', width = 12)),
                
tabPanel("Data Set", 
         titlePanel(HTML('SMRD data object: <code>printedcircuitboard</code>')),
         DT::dataTableOutput("printedcircuitboard")),
                
tabPanel("Figure 1.9",
  sidebarLayout( 
    sidebarPanel(width = 5,
      shinyAce::aceEditor(fontSize = 16, 
                                     wordWrap = T,
                                     outputId = "fig9plot", 
                          mode = "r", 
                          theme = "github", 
                          height = "450px",
                          value = 
"par(family = 'serif', font = 2)

plot(hoursl~rh, 
     data = SMRD::printedcircuitboard,
     pch = 'X', cex = .85, log = 'y',
     ylim = c(10,10000),xlim = c(45,85))

text(x = c(50,63,75,82), 
     y = c(7000,6000,1000,350), 
     labels = c('48/70 censored',
              '11/68 censored',
              '0/70 censored',
              '0/70 censored'))"),

        actionButton("evalfig9", "Evaluate")),
        
        mainPanel(plotOutput("plotfig9"), width = 7))))
