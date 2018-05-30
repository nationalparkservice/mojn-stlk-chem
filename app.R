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
    sidebarPanel(
      uiOutput("choose.chem.params")
    ),
    
    # Show one plot per lake of water chem parameter vs water year
    mainPanel(
      uiOutput("chem.plots"),
      tableOutput("test.table")
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
  
  # For testing purposes, output a data table of chem data filtered by the selected parameter
  output$test.table <- renderTable({
    filter(chem, CharacteristicLabel == input$chem.params)
  })
  
  # Generate one output plot per lake
  lakes <- levels(chem$Site)
  output$chem.plots <- renderUI({
    tagList(
      lapply(lakes, function(lake) {
        strong(lake)
      })
    )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

