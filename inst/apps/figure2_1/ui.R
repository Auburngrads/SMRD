ui = fluidPage(theme = teachingApps::add_theme(getShinyOption("theme")), 
               teachingApps::add_css(),

sidebarLayout( 
  sidebarPanel(width = 5,
    shinyAce::aceEditor(fontSize = 16, 
                        wordWrap = T,
                        outputId = "fig1plot", 
                        mode = "r", 
                        theme = "github", 
                        height = "450px",
                        value = 
"par(family = 'serif',font = 2)

library(package = SMRD)

distribution.plot('Weibull',
                  shape = c(1.7), 
                  scale = 1,
                  prob.range=c(.000001,.99))"),

        actionButton("evalfig1", "Evaluate")),
        
        mainPanel(plotOutput("plotfig1"), width = 7)))
