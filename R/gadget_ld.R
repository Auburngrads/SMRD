#' SMRD Analysis Shiny Gadget
#'
#' @description Shiny gadget used to visually inspect column values in a data set 
#' and subset rows by specifying column values
#'
#' @param data A data set
#' @param theme \code{character} A bootswatch theme provided to \code{shinythemes::shinytheme}
#' @param width \code{character} Width of the gadget (in valid css units)
#' @param height \code{character} Height of the gadget (in valid css units)
#' @param css \code{character} Path to a custom css file
#' 
#' @importFrom shinythemes shinytheme 
#' @importFrom shiny runGadget browserViewer
#' @importFrom shiny fluidPage tags includeCSS sidebarLayout sidebarPanel
#' @importFrom shiny uiOutput selectizeInput actionButton reactive h4
#' @importFrom shiny stopApp observeEvent mainPanel
#' @importFrom data.table as.data.table
#' @importFrom DT renderDataTable dataTableOutput datatable
#' 
#' @return A \code{list} of length 4
#'   \item{obj}{A \code{life.data} object}
#'   \item{dist}{The selected distribution}
#'   \item{cdfest}{A cdfest object}
#'   \item{mlest}{A mlest object}
#' 
#' @examples \dontrun{gadget_ld(data = shockabsorber)}
#' 
#' @export
gadget_ld <- 
function(data, theme = "flatly",
         width = '100%', height = '600px', css = NULL) {

if(is.null(css)) css <- system.file('resources','css','teachingApps.css',
                                    package = 'teachingApps')

ui = navbarPage(title = 'SMRD Survival Analysis',
                collapsible = T, 
                position = 'fixed-top',
                theme = shinythemes::shinytheme(theme = theme),
                header = tags$head(includeCSS(css)),

tabPanel(h4('Data Set'),
  sidebarLayout(
    sidebarPanel(width = 3,
                 selectInput('response', 
                             h4('Select Response Columns'),
                             multiple = T,
                             choices = colnames(data),
                             selected = NULL),
                 selectInput('censor', 
                             h4('Select Censor Column'),
                             multiple = T,
                             choices = colnames(data),
                             selected = NULL),
                 selectInput('caseweight', 
                             h4('Select Case Weight Column'),
                             multiple = T,
                             choices = colnames(data),
                             selected = NULL),
                 selectInput('xvars', 
                             h4('Select Explanatory Variables'),
                             multiple = T,
                             choices = colnames(data),
                             selected = NULL),
                 selectInput('failmodes', 
                             h4('Select Failure Mode Column'),
                             multiple = T,
                             choices = colnames(data),
                             selected = NULL)),
    
    mainPanel(width = 9,
      tabsetPanel(
        tabPanel(h4('Data Table'), dataTableOutput('datacolumns')),
        tabPanel(h4('Data Summary'), verbatimTextOutput('summary')))))),

tabPanel(h4("Event Plots"),
  sidebarLayout(
    sidebarPanel(width = 3,
      selectInput("eventplot", 
                  label = h2("Plot Type:"),
                  choices = c("Event Plot", "Histogram"),
                  selected = "Event Plot"),
      
      htmlOutput('breaks')),  
      
      mainPanel(width = 9, plotOutput("plotevent", height = "565px")))),

tabPanel(h4("Model Plots"),
  sidebarLayout(
    sidebarPanel(width = 3,
        selectInput("dist", 
                    label = h2("Distribution:"),
                    choices = c("Weibull",
                                "Exponential",
                                "Normal",
                                "Lognormal",                                                
                                "Smallest Extreme Value",
                                "Largest Extreme Value",
                                "Frechet"), 
                    selected = "Weibull"),
        
        sliderInput("ci",   
                    label = h2("Confidence Level:"),
                    max = 0.95,
                    min = 0.50,
                    step = 0.05, 
                    value = 0.95),
        
        selectInput("bt",   
                    label = h2("Band Type:"),
                    choices = c("Pointwise", "Simultaneous", "none"), 
                    selected = "Pointwise"),
        
        selectInput("mleplot", 
                  label = h2("Plot Type:"),
                  choices = c("CDF Plot", "Hazard Plot"), 
                  selected = "CDF Plot"),
        
        selectInput("paramloc", 
                  label = h2("Parameter Location:"),
                  choices = c("topleft",
                              "topright",
                              "bottomleft",
                              "bottomright"), 
                  selected = "bottomright"),
        
        selectInput("altvar", 
                  label = h2("Variance:"),
                  choices = c("Non Constant",
                              "Constant"), 
                  selected = "Non Constant")),
    
      mainPanel(width = 9,
        tabsetPanel(
          tabPanel(h4('CDF Plot'), plotOutput("cdfplot",height = "565px")),
          tabPanel(h4('MLE Plots'), plotOutput("mleplot",height = "565px"))
          #tabPanel(h2('ALT Plots'), plotOutput("altplot",height = "565px"))
          )))),
  
tabPanel(h4("Return Results"),
  sidebarLayout(
    sidebarPanel(width = 3,
        actionButton('stop', h4('Finish'))),
    mainPanel(width = 9,
              plotOutput('stiff')))))
    
server = function(input, output, session) {

  do.call('library', args = list('SMRD'))
  par(family = "serif",font = 2)

life.data <- reactive({ 
  
resps  <- which(colnames(data)%in%input$response)
status <- which(colnames(data)%in%input$censor)
cases  <- which(colnames(data)%in%input$caseweight)
xcols  <- which(colnames(data)%in%input$xvars)
fails  <- which(colnames(data)%in%input$failmodes)

  SMRD::frame.to.ld(frame = data,
                    response.column = `if`(length(resps)<1, NULL,resps),
                    censor.column = `if`(length(status)<1, NULL,status),
                    case.weight.column = `if`(length(cases)<1, NULL,cases),
                    x.columns = `if`(length(xcols)<1, NULL, xcols),
                    failure.mode.column = `if`(length(fails)<1, NULL, fails)) 
  })

output$datacolumns <- renderDataTable({ data })
output$summary  <- renderPrint({ summary(life.data()) })
  
observe({

  if(input$mleplot=='Compare CDF Plots') {

  output$compare <- renderUI({


selectInput("mlecomp", label = h2("Compare Distribution"),
                      choices = c("Weibull",
                                  "Exponential",
                                  "Normal",
                                  "Lognormal",
                                  "Smallest Extreme Value",
                                  "Largest Extreme Value",
                                  "Frechet"),
                      selected = "Lognormal")
})

  } else { output$compare <- renderUI({

selectInput("cimle",
            label = h2(HTML("<b>Confidence Level:</b>")),
            choices = c(0.99, 0.95, 0.90, 0.85, 0.80, 0.50),
            value = 0.95)
})
}
  
if(input$eventplot == 'Histogram') {

output$breaks <- renderUI({

      sliderInput('histbreaks',
                  h2('Number of Bins'),
                  min = 5,
                  max = 50,
                  step = 5,
                  value = 5)
})
} else { output$breaks <- renderUI({NULL }) }
})

output$plotevent <- renderPlot({

  if (input$eventplot == "Event Plot") event.plot(life.data())

  if (input$eventplot == "Histogram") hist(Response(life.data()),
                                      probability = TRUE,
                                      col = 1,
                                      border = "white",
                                      main = "",
                                      xlab = attr(life.data(),"time.units"),
                                      breaks = input$histbreaks,
                                      las = 1)
})
output$cdfplot <- renderPlot({

  plot(life.data(),
       distribution = input$dist,
       conf.level = as.numeric(input$ci),
       band.type = input$bt)
})
output$mleplot <- renderPlot({

  if (input$mleplot == "CDF Plot") mleprobplot(life.data(),
                                               distribution = input$dist[1],
                                               conf.level = as.numeric(input$ci),
                                               param.loc = input$paramloc)
  if (input$mleplot == "Hazard Plot") mlehazplot(life.data(),
                                                 distribution = input$dist[1],
                                                 conf.level = as.numeric(input$ci),
                                                 param.loc = input$paramloc)
  if (input$mleplot == "Compare CDF Plots")
    compare.mleprobplot(life.data(),
                        main.distribution = input$dist[1],
                        compare.distribution = `if`(length(input$dist)==1,
                                                    input$dist[1],
                                                    input$dist[-1]))
})
#   output$altplot <- renderPlot({
#
#   if (input$altvar == "Non Constant") groupi.mleprobplot(life.data(),
#                                                          distribution = input$dist[1],
#                                                          group.var = c(1, 2))
#   if (input$altvar == "Constant")     groupm.mleprobplot(life.data(),
#                                                          distribution = input$dist[1])
# })

observeEvent(input$stop, {

    stopApp(list(obj = life.data(),
                 dist = input$dist[1],
                 cdfest = print(SMRD::cdfest(data.ld = life.data())),
                 mlest = print(SMRD::mlest(data.ld = life.data(), distribution = input$dist[1]))))

})

output$stiff <- renderPlot({ plot(NULL, xlim = c(0,3), ylim = c(0,3)) })
}

shinygadgets::runGadget(app = ui,
                        server = server,
                        viewer = shinygadgets::browserViewer(browser = getOption("browser")))
}
