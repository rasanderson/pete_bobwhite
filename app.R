#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Load necessary libraries
library(shiny)
library(ggplot2)
library(plotly)
library(htmlwidgets)
library(vegan)


# Define UI for application
ui <- fluidPage(
  # Application title
  titlePanel("Interactive ggplot2 geom_point graph"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    sidebarPanel(
      # Placeholder for controls
      checkboxGroupInput("site_choice", "Filter by site no:",
                         choices = rep(paste0("no", sprintf("%02d", 1:16))),
                         selected = rep(paste0("no", sprintf("%02d", 1:8))))
    ),
    
    # Main panel for displaying outputs
    mainPanel(
      plotlyOutput("plot") 
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Define a vector of sound files
  sounds <- list.files(path = "calls", recursive = TRUE, pattern = "\\.wav$", full.names = TRUE)

  output$plot <- renderPlotly({
    sounds <- sounds[grepl(paste(input$site_choice, collapse = "|"), sounds)]
    
    # Create a data frame with 3 points
    df <- readRDS("mfcc_df.RDS")
    df <- df[df$Class %in% input$site_choice,]
    df_pca <- rda(df[, -c(1)], scale = TRUE)
    df_sco <- scores(df_pca, display="sites")
    
    # Create a ggplot2 geom_point graph
    p <- ggplot(df_sco, aes(x = PC1, y = PC2, colour = df[,1])) +
      geom_point()
    
    # Convert ggplot2 to plotly
    gp <- ggplotly(p)
    
    # Add click event
    gp <- gp %>% layout(
      autosize = F,
      hovermode = "closest",
      dragmode = "select",
      showlegend = T,
      margin = list(r = 10, t = 25, b = 40, l = 60),
      annotations = list(
        text = "Click on point to play sound",
        showarrow = F,
        xref = "paper", yref = "paper",
        x = 0.005, y = -0.002 )
    )
    
    # Pass the sounds variable to the JavaScript code
    jsCode <- sprintf("
      function(el, x) {
        var sounds = %s;
        el.on('plotly_click', function(data) {
          console.log('Point clicked:', data);
          var pointIndex = data.points[0].pointNumber;
          var soundFile = sounds[pointIndex];
          console.log('Playing sound:', soundFile);
          Shiny.onInputChange('playSound', soundFile);
        });
      }
    ", jsonlite::toJSON(sounds))
    
    gp <- gp %>% onRender(jsCode)
    
    return(gp)
  })
  
  observeEvent(input$playSound, {
    print(paste("Playing sound:", input$playSound))
    system(paste("aplay", input$playSound))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
