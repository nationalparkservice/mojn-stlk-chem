library(shiny)
library(plotly)
library(RODBC)
library(tidyverse)

# Define UI
ui <- fluidPage(
  
  # Application title
  titlePanel("MOJN Lake Chemistry"),
  
  sidebarLayout(
    # Sidebar with a dropdown to select water chemistry parameter
    sidebarPanel(width = 3,
      uiOutput("choose.chem.params")
    ),
    
    # Show one plot per lake of water chem parameter vs water year
    mainPanel(
      uiOutput("chem.plots")
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Connect to the database
  LAKES.dsn <- 'driver={SQL Server Native Client 11.0};server=INPLAKE52V\\MOJN;database=MOJN_Lakes;trusted_connection=Yes;applicationintent=readonly'
  LAKES.db <- odbcDriverConnect(LAKES.dsn)
  # Pull water chem data into a dataframe
  chem <- sqlFetch(LAKES.db, "analysis.WaterChemistry")
  # Close database connection
  close(LAKES.db)
  
  # Generate water chem parameter select input
  chem.choices <- levels(chem$CharacteristicLabel)
  output$choose.chem.params <- renderUI({
    selectInput("chem.params", label = "Choose a water chemistry parameter", choices = chem.choices)
  })
  
  # Get list of lakes
  lakes <- levels(chem$Site)
  rows <- seq(from = 1, to = length(lakes), by = 2)
  
  # Insert one Plotly output object per lake into the UI
  output$chem.plots <- renderUI({
    plot.outputs <- lapply(rows, function(i) {
      fluidRow(
        column(6, plotlyOutput(lakes[i])),
        column(6, plotlyOutput(lakes[i+1]))
      )
    })
    tagList(plot.outputs)
  })
  
  # Actually render the plots
  for (lake in lakes) {
    local({
      my.lake <- lake
      output[[my.lake]] <- renderPlotly({
        filter(chem, (Site == my.lake) & (CharacteristicLabel == input$chem.params)) %>%
          arrange(Site, VisitDate) %>%
          plot_ly(x = ~VisitDate,
                  y = ~LabValue,
                  color = ~SampleType,
                  type = "scatter",
                  mode = "lines+markers",
                  text = ~paste("Visit date: ", VisitDate, "<br>Flag: ", DQF, "<br>Note: ", DQFNote, "<br>DPL :", DPL)) %>%
          layout(title = my.lake,
                 xaxis = list(title = ""))
      })
    })
  }
}

# Run the application 
shinyApp(ui = ui, server = server)

