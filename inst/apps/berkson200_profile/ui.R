ui = fluidPage(theme = SMRD::add_theme(getShinyOption("theme")), 
               SMRD::add_css(),
      
sidebarLayout(
   sidebarPanel(width = 5,
      shinyAce::aceEditor(fontSize = 16, 
                          wordWrap = T,
                          outputId = "berkprofile200", 
                          mode = "r", 
                          theme = "github", 
                          height = "450px", 
                          value = 
"par(family  = 'serif')

library(SMRD)

berkson200.ld <- 
frame.to.ld(berkson200,
            response.column = c(1,2),
            censor.column = 3,
            case.weight.column = 4,
            time.units = '1/5000 Seconds')

simple.contour(berkson200.ld, 
               distribution = 'exponential', 
               xlim = c(450,800))"),

        actionButton("berk200profiles", "Evaluate")),
        
        mainPanel(plotOutput("berk200prof"), width = 7)))
