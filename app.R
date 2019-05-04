#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shinydashboard)
library(readr)
library(plotly)

human <- read_csv("data/human.csv", col_types = cols(date = col_datetime(format = "%Y-%m-%d")))
mouse <- read_csv("data/mouse.csv", col_types = cols(date = col_datetime(format = "%Y-%m-%d")))


header <- dashboardHeader(
  title="IMGT Germline Updates"
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Home", tabName = "home", icon=icon("dashboard")),
    menuItem("Germlines", 
             menuSubItem("Homo sapiens",tabName = "human", icon = icon("link")),
             menuSubItem("Mouse",tabName = "mouse", icon = icon("link")),
             startExpanded = TRUE,
             tabName = "germlines", icon = icon("list")),
    menuItem("Source code", icon = icon("file-code-o"), 
             href = "https://github.com/3rand/imgt-updates")
    
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "home",
            h3("Welcome to the IMGT germline updates tracker"),
            p("This tool contains the statistics of the changes made to the IMGT reference directory for human an mouse germlines."),
            p("The changes are categorized as one of: "),
            p("removals"),
            p("gene addition"),
            p("allelle addition (whenever a gene is added or multiple allelles of existing gene are added)"),
            p("sequence update (whenever the reference nt sequence of genes was changed"),
            p("metadata update (whenever anything except for nt sequence was changed, eg name, functionality, numberin etc)"),
            p(" "),
            p("The data files along with the source code are maintained over GitHub. Please post an issue there or make a pull request with your recommended changes."),
            p("Currently the focus is to build up the list of all species as well as maintaining an updated reference.")
    ),
    tabItem(tabName = "human",
            h3("Germline changes for: human, homo sapiens, hs"),
            fluidRow(
              box(
                width = 12,
                plotlyOutput("humanplot1")
                
              ),
              box(
                width = 12,
                title = "Origin selection",
                sliderInput("sliderhuman", "Date", min(human$date), max(human$date), max(human$date), timeFormat="%Y-%m-%d")
              )
              
            )
    ),
    tabItem(tabName = "mouse",
            h2("Germline changes for: mouse, mus musculus"),
            fluidRow(
              box(
                width = 12,
                plotlyOutput("mouseplot1")
                
              ),
              box(
                width = 12,
                title = "Origin selection",
                sliderInput("slidermouse", "Date", min(human$date), max(human$date), max(human$date), timeFormat="%Y-%m-%d")
              )
              
            )
    )
  )
)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  skin = "purple",
  header,
  sidebar,
  body
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  human <- read_csv("data/human.csv", col_types = cols(date = col_datetime(format = "%Y-%m-%d")))
  
  makeCumulatives <- function(data,zerodate) {
    start <- which(human$date <= zerodate)[1];
    data$cummulative_removals = 0;
    data$cummulative_geneAdditions = 0;
    data$cummulative_alelleAdditions = 0;
    data$cummulative_sequnceUpdates = 0;
    data$cumulative_metadataUpdates = 0;
    
    if (start<nrow(data)) {
      for (i in c((start+1):nrow(data))) {
        data$cummulative_removals[i] <- sum(data$removals[1:(i-1)], na.rm = TRUE)
        data$cummulative_geneAdditions[i] <- sum(data$geneAdditions[1:(i-1)], na.rm = TRUE)
        data$cummulative_alelleAdditions[i] <- sum(data$allelleAdditions[1:(i-1)], na.rm = TRUE)
        data$cummulative_sequnceUpdates[i] <- sum(data$sequenceUpdate[1:(i-1)], na.rm = TRUE)
        data$cumulative_metadataUpdates[i] <- sum(data$metadataUpdate[1:(i-1)], na.rm = TRUE)
      }
    }
    
    if (start>1) {
      for (i in c((start-1):1)) {
        data$cummulative_removals[i] <- sum(data$removals[i:start], na.rm = TRUE)
        data$cummulative_geneAdditions[i] <- sum(data$geneAdditions[i:start], na.rm = TRUE)
        data$cummulative_alelleAdditions[i] <- sum(data$allelleAdditions[i:start], na.rm = TRUE)
        data$cummulative_sequnceUpdates[i] <- sum(data$sequenceUpdate[i:start], na.rm = TRUE)
        data$cumulative_metadataUpdates[i] <- sum(data$metadataUpdate[i:start], na.rm = TRUE)
      }
      
      data$cummulative_removals[1:start] = 0 - data$cummulative_removals[1:start];
      data$cummulative_geneAdditions[1:start] = 0 - data$cummulative_geneAdditions[1:start];
      data$cummulative_alelleAdditions[1:start] = 0 - data$cummulative_alelleAdditions[1:start];
      data$cummulative_sequnceUpdates[1:start] = 0 - data$cummulative_sequnceUpdates[1:start];
      data$cumulative_metadataUpdates[1:start] = 0 - data$cumulative_metadataUpdates[1:start];
      
    }
    
    data;
    
      
  }
  
   
  
   output$humanplot1 <- renderPlotly({
     zerodate = input$sliderhuman;
     data <- makeCumulatives(human,zerodate);
     p <- plot_ly(data, x = ~date, y = ~cummulative_removals, name = 'Removals', type = 'scatter', mode = 'none', stackgroup = 'one', fillcolor = '#F5FF8D', text=~releaseName) %>%
       add_trace(y = ~cummulative_geneAdditions, name = 'Gene Additions', fillcolor = '#50CB86') %>%
       add_trace(y = ~cummulative_alelleAdditions, name = 'Alelle Additions', fillcolor = '#4C74C9') %>%
       add_trace(y = ~cummulative_sequnceUpdates, name = 'Sequence Updates', fillcolor = '#700961') %>%
       add_trace(y = ~cumulative_metadataUpdates, name = 'Metadata Updates', fillcolor = '#312F44') %>%
       layout(
         xaxis = list(title = 'Date'),
         yaxis = list(title = 'Cummulative changes')
         )
     
     p
   })
   
   output$mouseplot1 <- renderPlotly({
     zerodate = input$slidermouse;
     data <- makeCumulatives(mouse,zerodate);
     p <- plot_ly(data, x = ~date, y = ~cummulative_removals, name = 'Removals', type = 'scatter', mode = 'none', stackgroup = 'one', fillcolor = '#F5FF8D', text=~releaseName) %>%
       add_trace(y = ~cummulative_geneAdditions, name = 'Gene Additions', fillcolor = '#50CB86') %>%
       add_trace(y = ~cummulative_alelleAdditions, name = 'Alelle Additions', fillcolor = '#4C74C9') %>%
       add_trace(y = ~cummulative_sequnceUpdates, name = 'Sequence Updates', fillcolor = '#700961') %>%
       add_trace(y = ~cumulative_metadataUpdates, name = 'Metadata Updates', fillcolor = '#312F44') %>%
       layout(
         xaxis = list(title = 'Date'),
         yaxis = list(title = 'Cummulative changes')
       )
     
     p
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

