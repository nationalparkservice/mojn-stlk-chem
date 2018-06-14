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
  
  # # Connect to MOJN Lakes database, import all water chem data, and write it to a .csv
  # LAKES.dsn <- 'driver={SQL Server Native Client 11.0};server=INPLAKE52V\\MOJN;database=MOJN_Lakes;trusted_connection=Yes;applicationintent=readonly'
  # LAKES.db <- odbcDriverConnect(LAKES.dsn)
  # # Pull water chem data into a dataframe
  # chem <- sqlFetch(LAKES.db, "analysis.WaterChemistry")
  # chem <- spread(chem, SampleType, LabValue)
  # names(chem) <- sub(" ", "", names(chem))
  # # Close database connection
  # close(LAKES.db)
  # write.csv(chem, "chem_data.csv", row.names = FALSE)
  
  # Read data from csv
  chem <- read.csv("chem_data.csv")
  
  # Convert dates from factor to date
  chem$VisitDate <- as.Date(chem$VisitDate)
  
  # Create a column that includes the water chem characteristic with the corresponding units
  chem$CharacteristicWUnit <- paste0(chem$CharacteristicLabel, " (", chem$Unit, ")") %>%
    as.factor()
  
  # Generate water chem parameter select input
  chem.choices <- levels(chem$CharacteristicWUnit)
  output$choose.chem.params <- renderUI({
    selectInput("chem.params", label = "Choose a water chemistry parameter", choices = chem.choices, selected = "Alkalinity2 (mg CaCO3/L)")
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
      # Filter chem data by lake and selected water chem parameter
      my.lake <- lake
      
      # Render plot for the given lake
      output[[my.lake]] <- renderPlotly({
        # Get selected water chem parameter from the input dropdown
        my.param <- input$chem.params
        # Get x and y ranges for data across all lakes so that all plots can have the same axes
        param.data <- filter(chem, CharacteristicWUnit == my.param)
        max.date <- max(param.data$VisitDate, na.rm = TRUE)
        min.date <- min(param.data$VisitDate, na.rm = TRUE)
        max.y <- max(rbind(param.data$Routine, param.data$LabDuplicate), na.rm = TRUE)
        min.y <- min(rbind(param.data$Routine, param.data$LabDuplicate), na.rm = TRUE)
        
        buffer.date <- ceiling(0.05*(max.date - min.date))
        range.date <- c(min.date - buffer.date, max.date + buffer.date)
        buffer.y <- 0.05*(max.y - min.y)
        range.y <- c(min.y - buffer.y, max.y + buffer.y)
        
        # Filter by lake and chemistry parameter
        data.to.plot <- filter(chem, (Site == my.lake) & (CharacteristicWUnit == my.param)) %>%
          arrange(Site, VisitDate)
        xrange <- c(min(data.to.plot$VisitDate), max(data.to.plot$VisitDate))
        
        p <- plot_ly(data = data.to.plot,
                x = ~VisitDate,
                y = ~Routine,
                type = "scatter",
                mode = "lines+markers",
                name = "Primary",
                text = ~ifelse(is.na(VisitDate), NA, paste("Visit date: ", VisitDate, "<br>Flag: ", DQF, "<br>Note: ", DQFNote, "<br>Data Processing Level: ", DPL))) %>%
          add_trace(y = ~LabDuplicate,
                    name = "Duplicate",
                    mode = "markers") %>%
          layout(title = my.lake,
                 xaxis = list(title = "",
                              range = range.date),
                 yaxis = list(title = my.param,
                              range = range.y))
        p$elementId <- NULL
        p
      })
    })
  }
}

# Run the application
shinyApp(ui = ui, server = server)

