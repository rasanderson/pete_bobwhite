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

# load necessary libraries
library(shiny)
library(ggplot2)
library(png)

spec_files <- list.files(path = "spectrogram", recursive = TRUE, pattern = "\\.png$", full.names = TRUE)

# define UI
ui <- fluidPage(
  splitLayout(
    plotOutput("scatterplot"),
    imageOutput("image")
  )
)

# define server logic
server <- function(input, output) {
  # create a data frame with 10 points
  df <- data.frame(x = rnorm(10), y = rnorm(10))
  
  # list of image files
  #image_files <- c("image1.png", "image2.png", "image3.png", "image4.png", "image5.png", 
  #                 "image6.png", "image7.png", "image8.png", "image9.png", "image10.png")
  image_files <- spec_files[1:10]
  
  # create scatterplot
  output$scatterplot <- renderPlot({
    ggplot(df, aes(x, y)) +
      geom_point(size = 4) +
      theme_minimal()
  })
  
  # display image when point is clicked
  output$image <- renderImage({
    # get clicked point
    click <- input$scatterplot_click
  #  if (is.null(click)) return(NULL)

    # find closest point
    dists <- sqrt((df$x - click$x)^2 + (df$y - click$y)^2)
    closest_point <- which.min(dists)
    renderPrint(closest_point)
  #   
  #   # return image associated with closest point
  #   # if (!is.null(closest_point)) {
  #   #   list(src = image_files[closest_point],
  #   #        contentType = "image/png",
  #   #        width = 400,
  #   #        height = 300)
  #   # } else {
  #   #   return(NULL)
  #   # }
    list(src = image_files[1],
         contentType = "image/png",
         width = 400,
         height = 300)
  },
    deleteFile = FALSE)

}

# run the application 
shinyApp(ui = ui, server = server)






# # Define UI for application
# ui <- fluidPage(
#   # Application title
#   titlePanel("Interactive ggplot2 geom_point graph"),
#   
#   # Sidebar layout with input and output definitions
#   sidebarLayout(
#     sidebarPanel(
#       # Placeholder for controls
#       checkboxGroupInput("site_choice", "Filter by site no:",
#                          choices = rep(paste0("no", sprintf("%02d", 1:16))),
#                          selected = rep(paste0("no", sprintf("%02d", 1:8))))
#     ),
#     
#     # Main panel for displaying outputs
#     mainPanel(
# #      plotlyOutput("plot"),
#       plotOutput("plot"),
#       # fluidRow(
#       #   column(width = 3,
#       #          verbatimTextOutput("hover_info"))
#       # )
#     )
#   )
# )
# 
# # Define server logic
# server <- function(input, output, session) {
#   # Define a vector of sound files
#   sounds <- list.files(path = "filtered", recursive = TRUE, pattern = "\\.wav$", full.names = TRUE)
# 
#   # output$plot <- renderPlotly({
#   #   sounds <- sounds[grepl(paste(input$site_choice, collapse = "|"), sounds)]
#   output$plot <- renderPlot({    
#     # Create a data frame with 3 points
#     df <- readRDS("mfcc_df.RDS")
#     df <- df[df$Class %in% input$site_choice,]
#     df_pca <- rda(df[, -c(1)], scale = TRUE)
#     df_sco <- data.frame(scores(df_pca, display="sites"))
#     
#     # Create a ggplot2 geom_point graph
#     ggplot(df_sco, aes(x = PC1, y = PC2, colour = df[,1])) +
#       geom_point()
#     
#     # Convert ggplot2 to plotly
#     # gp <- ggplotly(p)
#     # 
#     # # Add click event
#     # gp <- gp %>% layout(
#     #   autosize = F,
#     #   hovermode = "closest",
#     #   dragmode = "select",
#     #   showlegend = T,
#     #   margin = list(r = 10, t = 25, b = 40, l = 60),
#     #   annotations = list(
#     #     text = "Click on point to play sound",
#     #     showarrow = F,
#     #     xref = "paper", yref = "paper",
#     #     x = 0.005, y = -0.002 )
#     # )
#     # 
#     # # Pass the sounds variable to the JavaScript code
#     # jsCode <- sprintf("
#     #   function(el, x) {
#     #     var sounds = %s;
#     #     el.on('plotly_click', function(data) {
#     #       console.log('Point clicked:', data);
#     #       var pointIndex = data.points[0].pointNumber;
#     #       var soundFile = sounds[pointIndex];
#     #       console.log('Playing sound:', soundFile);
#     #       Shiny.onInputChange('playSound', soundFile);
#     #     });
#     #   }
#     # ", jsonlite::toJSON(sounds))
#     # 
#     # gp <- gp %>% onRender(jsCode)
#     
#     # output$hover_info <- renderPrint({
#     #   cat("printing something \n")
#     # })
# 
#     return(p)
#   })
#   
#   observeEvent(input$playSound, {
#     print(paste("Playing sound:", input$playSound))
#     system(paste("aplay", input$playSound))
#   })
# }
# 
# # Run the application 
# shinyApp(ui = ui, server = server)
