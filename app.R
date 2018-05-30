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
      
    ),
    
    # Show one plot per lake of water chem parameter vs water year
    mainPanel(
      
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
  chem.params <- levels(chem$CharacteristicLabel)
  
  # Get the water chem parameter from the dropdown
  
  # Generate one output plot per lake
  lakes <- levels(chem$Site)
  
}

# Run the application 
shinyApp(ui = ui, server = server)

