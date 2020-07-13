ui = navbarPage(title = 'Zelen Cap',
                collapsible = T, 
                position = 'fixed-top',
                theme  = SMRD::add_theme(getShinyOption('theme')),
                header = SMRD::add_css(),
                footer = SMRD::add_logo(),


    tabPanel("Data Set", DT::dataTableOutput("table2", height = "565px") ),    
    tabPanel("Summary",  verbatimTextOutput("summary2") ), 

    tabPanel("Event Plots",
      sidebarLayout(
      sidebarPanel(
        
      selectInput("PLOT2", 
                  label = "Plot Type:",
                  choices = c("Event Plot",
                              "Histogram"),
                  selected = "Event Plot")),  
      
      mainPanel( plotOutput("eventplot2", height = "565px")))),

    tabPanel("CDF Plot",
      sidebarLayout(
      sidebarPanel(
        
        selectInput("dist2", 
                    label = "Distribution:",
                    choices = c("Weibull",
                                "Exponential",
                                "Normal",
                                "Lognormal",                                                
                                "Smallest Extreme Value",
                                "Largest Extreme Value",
                                "Frechet"), 
                    selected = "Weibull"),
        
        selectInput("ci2",   
                    label = "Confidence Level:",
                    choices = c(0.99, 0.95, 0.90, 0.85, 0.80, 0.50), 
                    selected = 0.95),
        
        selectInput("bt2",   
                    label = "Band Type:",
                    choices = c("Pointwise", 
                                "Simultaneous", 
                                "none"), 
                    selected = "Pointwise")), 
      
      mainPanel( plotOutput("cdfplot2",height = "565px")))),
    
  tabPanel("MLE Plot",
    sidebarLayout(
    sidebarPanel(
      selectInput("mleplot", 
                  label = "Plot Type:",
                  choices = c("CDF Plot",
                              "Hazard Plot"), 
                  selected = "CDF Plot"),
      
      selectInput("mledist", 
                  label = "Distribution:",
                  choices = c("Weibull",
                              "Exponential",
                              "Normal",
                              "Lognormal",                                                
                              "Smallest Extreme Value",
                              "Largest Extreme Value",
                              "Frechet"), 
                  selected = "Weibull"),
      
      selectInput("paramloc", 
                  label = "Parameter Location:",
                  choices = c("topleft",
                              "topright",
                              "bottomleft",
                              "bottomright"), 
                  selected = "bottomright")),
    
    mainPanel( plotOutput("mleplot",height = "565px")))),

  tabPanel("ALT Plot",
    sidebarLayout(
    sidebarPanel(
      selectInput("altvar", 
                  label = "Variance:",
                  choices = c("Non Constant",
                              "Constant"), 
                  selected = "Non Constant"),
      
      selectInput("altdist", 
                  label = "Distribution:",
                  choices = c("Weibull",
                              "Exponential",
                              "Normal",
                              "Lognormal",                                                
                              "Smallest Extreme Value",
                              "Largest Extreme Value",
                              "Frechet"), 
                  selected = "Weibull")),
    
    mainPanel( plotOutput("altplot",height = "565px")))))
