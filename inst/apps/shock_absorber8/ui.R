ui = navbarPage(title = 'Shock Absorber',
                collapsible = T, 
                position = 'fixed-top',
                theme  = SMRD::add_theme(getShinyOption('theme')),
                header = SMRD::add_css(),
                footer = SMRD::add_logo(),
                
                
tabPanel("Data Set",   DT::dataTableOutput("table.shock", height = "80%") ),    
tabPanel("Summary", verbatimTextOutput("summary.shock")), 
tabPanel("Event Plots",
  sidebarLayout(
    sidebarPanel(width = 3,
      selectInput("PLOT_3", 
                  label = "Plot Type",
                  choices = c("Event Plot","Histogram"),
                  selected = "Event Plot")),  
    
mainPanel( plotOutput("eventplot.shock"), width = 9))),

tabPanel("CDF Plot",
  sidebarLayout(
    sidebarPanel(width = 3,
      selectInput("DIST_3", 
                  label = "Distribution",
                  choices = c("Weibull",
                              "Exponential",
                              "Normal",
                              "Lognormal",
                              "Smallest Extreme Value",
                              "Largest Extreme Value",
                              "Frechet"), 
                  selected = "Weibull"),

      selectInput("CI_3",
                  label = "Confidence Level",
                  choices = c(0.99, 0.95, 0.90, 0.85, 0.80, 0.50), 
                  selected = 0.95),
                   
      selectInput("BT_3",
                  label = "Band Type",
                  choices = c("Pointwise", 
                              "Simultaneous", 
                              "none"),
                  selected = "Pointwise")),  

mainPanel( plotOutput("cdfplot.shock"), width = 9))),

tabPanel("MLE Plot",
  sidebarLayout(
    sidebarPanel(width = 3,
      selectInput("mleplot", 
                  label = "Plot Type",
                  choices = c("CDF Plot",
                             "Hazard Plot", 
                             "Compare CDF Plots"), 
                  selected =  "CDF Plot"),

      selectInput("mledist", 
                  label = "Distribution",
                  choices = c("Weibull",
                              "Exponential",
                              "Normal",
                              "Lognormal",                       
                              "Smallest Extreme Value",
                              "Largest Extreme Value",
                              "Frechet"), 
                   selected = "Weibull"),

      selectInput("mlecomp", 
                  label = "Compare Distribution",
                  choices = c("Weibull",
                              "Exponential",
                              "Normal",
                              "Lognormal",                      
                              "Smallest Extreme Value",
                              "Largest Extreme Value",
                              "Frechet"), 
                  selected = "Weibull"),

      selectInput("paramloc", 
                  label = "Parameter Location",
                  choices = c("topleft",
                              "topright",
                              "bottomleft",
                              "bottomright"), 
                  selected = "bottomright")),  
    
mainPanel( plotOutput("mleplot"), width = 9))),

tabPanel('Code Mirror',
         
mainPanel(codemirrorR::codemirrorOutput('mlemirror'), width = 12)))
