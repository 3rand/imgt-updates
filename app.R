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
library(reshape2)

data <- read_csv("human.csv", col_types = cols(date = col_date(format = "%d. %B %Y")))
data[is.na(data)] = 0;
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("homo sapiens", tabName = "human", icon = icon("link")),
    menuItem("mus musculus", icon = icon("link"), tabName = "mouse")
  )
)


body <- dashboardBody(
  tabItems(
    tabItem(tabName = "human",
            h3("Homo sapiens germline")
    ),
    
    tabItem(tabName = "mouse",
            h3("Mus musculus germline")
    )
  ),
  fluidRow(
    box(
      width = 12, 
      title = "Cummulative germline changes", 
      br(),
      plotOutput("plot1_human", height = 600)
    ),
    box(
      width = 12, 
      title = "Change starting point", 
      sliderInput("slider", "Release date:", min=min(data$date), max=max(data$date), value=max(data$date))
    ),
    box(
      width = 12,
      title = "Raw data",
      tableOutput("rawdata")
    )
  )
)



# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "IMGT Germline historical comparison"),
  sidebar,
  body
)

server <- function(input, output) {
  
  humanData <- function(startdate) {
    data$cumulative_removals = 0;
    data$cumulative_geneAdditions = 0;
    data$cumulative_alelleAdditions = 0;
    data$cumulative_sequenceUpdates = 0;
    data$cumulative_metadataUpdates = 0;
    
    start <- which(data$date <= as.Date(startdate,"%Y-%m-%d"))[1]
    
    for (i in c(2:nrow(data))) {
      data$cumulative_removals[i] <- sum(data$removals[1:i])
      data$cumulative_removals[i] <- data$cumulative_removals[i] - sum(data$cumulative_removals[1:start])
      
      data$cumulative_geneAdditions[i] <- sum(data$geneAdditions[1:i])
      data$cumulative_geneAdditions[i] <- data$cumulative_geneAdditions[i] - sum(data$cumulative_geneAdditions[1:start])
      
      data$cumulative_alelleAdditions[i] <- sum(data$allelleAdditions[1:i])
      data$cumulative_alelleAdditions[i] <- data$cumulative_alelleAdditions[i] - sum(data$cumulative_alelleAdditions[1:start])
      
      data$cumulative_sequenceUpdates[i] <- sum(data$sequenceUpdate[1:i])
      data$cumulative_sequenceUpdates[i] <- data$cumulative_sequenceUpdates[i] - sum(data$cumulative_sequenceUpdates[1:start])
      
      data$cumulative_metadataUpdates[i] <- sum(data$metadataUpdate[1:i])
      data$cumulative_metadataUpdates[i] <- data$cumulative_metadataUpdates[i] - sum(data$cumulative_metadataUpdates[1:start])
    }
    
    data$cumulativeToDate <- data$cumulative_removals + data$cumulative_geneAdditions + data$cumulative_alelleAdditions + data$cumulative_sequenceUpdates + data$cumulative_metadataUpdates;
    
    data
  }
  
  
  
  
  
  output$plot1_human <- renderPlot({
    
    
    germlineData <- melt(humanData(input$slider), id.vars = c("date","releaseName"), na.rm = FALSE)
    modifynames <- function(vanames) {
      gsub("cumulative_", "", vanames)
    }
    ploti <- ggplot(
      germlineData[
        which(
          germlineData$variable %in% c("cumulative_removals","cumulative_geneAdditions","cumulative_alelleAdditions","cumulative_sequenceUpdates","cumulative_metadataUpdates")
          ),], aes(x=date, y=value, fill=variable)) +
      geom_area(colour="black", size=.2, alpha=.4) +
      scale_fill_brewer(palette="RdYlBu", breaks=levels(germlineData$variable), name="Type of change", labels=modifynames(levels(germlineData$variable))) +
      theme(legend.position="top") +
      geom_area();
    
    ploti + 
      labs(
        title=input$slider,
        x = "Release date", 
        y = "Nr. Changes", 
        caption = "A historical event plot of updates accumulated to the reference directory for 10 years.\nThe horizontal axis represents the date of release and the vertical axis represents the ammount of changes coloured by type of change as compared to the latest germline as of this study.\nFor any 2 past dates x1 and x2 we can observe the number of changes |y2-y1| between the latest germiles as of those dates.") +
      scale_x_date(date_breaks = "3 months", date_minor_breaks = "month", date_labels = "%b %Y", sec.axis = dup_axis(name="Release name", breaks = data$date, labels = data$releaseName)) +
      
      theme(axis.text.x = element_text(angle = 90))
    
    
  })
  
  output$rawdata <- renderDataTable(
    humanData(input$slider)
    )
}

# Run the application 
shinyApp(ui = ui, server = server)

