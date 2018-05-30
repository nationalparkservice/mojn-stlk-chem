library(shiny)
library(plotly)

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
  
  # Pull water chem data into a dataframe
  
  # Generate water chem parameter select input
  
  # Get the water chem parameter from the dropdown
  
  # Generate one output plot per lake
  
}

# Run the application 
shinyApp(ui = ui, server = server)

