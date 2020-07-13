ui = fluidPage(theme = SMRD::add_theme(getShinyOption("theme")), 
               SMRD::add_css(),
     
sidebarLayout(           
  sidebarPanel(width = 3,           
    selectInput("n7.2", 
                label = "Observations",
                choices = c(20, 200, 2000, 10220), 
                selected = 20),
  
    selectInput("ci7.2", 
                label = "Confidence Level",
                choices = c("50","80","90","95","99"), 
                selected = "95")),
  
  mainPanel(plotOutput('berksonrel'), width = 9)))
