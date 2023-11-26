#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Load necessary libraries
library(shiny)
library(ggplot2)
library(plotly)
library(htmlwidgets)


# Load necessary libraries
library(shiny)
library(ggplot2)
library(plotly)

# Define UI for application
ui <- fluidPage(
  # Application title
  titlePanel("Interactive ggplot2 geom_point graph"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    sidebarPanel(
      # Placeholder for controls
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
  sounds <- c("/usr/share/sounds/alsa/Front_Center.wav", "/usr/share/sounds/alsa/Rear_Center.wav", "/usr/share/sounds/alsa/Side_Left.wav")
  
  output$plot <- renderPlotly({
    # Create a data frame with 3 points
    df <- data.frame(x = c(1, 2, 3), y = c(1, 2, 3))
    
    # Create a ggplot2 geom_point graph
    p <- ggplot(df, aes(x = x, y = y)) +
      geom_point()
    
    # Convert ggplot2 to plotly
    gp <- ggplotly(p)
    
    # Add click event
    gp <- gp %>% layout(
      autosize = F,
      hovermode = "closest",
      dragmode = "select",
      showlegend = F,
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