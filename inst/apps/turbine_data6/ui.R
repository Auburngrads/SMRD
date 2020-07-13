ui = navbarPage(title = 'Turbine Data Example',
                collapsible = T, 
                position = 'fixed-top',
                theme  = SMRD::add_theme(getShinyOption('theme')),
                header = SMRD::add_css(),
                footer = SMRD::add_logo(),


tabPanel("Data Set",  DT::dataTableOutput("table.turb", height = "80%") ),    
tabPanel("Summary", verbatimTextOutput("summary.turb") ), 
tabPanel("Event Plots",
  sidebarLayout(
    sidebarPanel(width = 3,
      selectInput("PLOT_5", 
                  label = "Plot:",
                  choices = c("Event Plot",
                              "Histogram"),
                  selected = "Event Plot")),  
    
mainPanel( plotOutput("eventplot.turb", height = '650px'), width = 9))),

tabPanel("CDF Plot",
  sidebarLayout(
    sidebarPanel(width = 3,
      selectInput("DIST_5", 
                  label = "Distribution:",
                  choices = c("Weibull",
                              "Exponential",
                              "Normal",
                              "Lognormal",
                              "Smallest Extreme Value",
                              "Largest Extreme Value",
                              "Frechet"), 
                  selected = "Weibull"),

      selectInput("CI_5",   
                  label = "Confidence Level:",
                  choices = c(0.99, 0.95, 0.90, 0.85, 0.80, 0.50), 
                  selected = 0.95),
                   
      selectInput("BT_5",
                  label = "Band Type:",
                  choices = c("Pointwise", 
                              "Simultaneous", 
                              "none"), 
                  selected = "Pointwise")),  
    
mainPanel( plotOutput("cdfplot.turb", height = '650px'), width = 9))))

