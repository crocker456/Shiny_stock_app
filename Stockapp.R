library(shiny)
library(plotly)
library(tidyquant)
library(ggplot2)
library(dplyr)

symbols = c("VFC", "SQBG", "NKE", "ICON", "ZUMZ", "ADS")

skate = symbols %>%  tq_get(get = "stock.prices", from = " 2002-01-01")

# Define UI for application that plots features of movies
ui <- fluidPage(
  
  # Sidebar layout with a input and output definitions
  sidebarLayout(
    
    # Inputs
    sidebarPanel(
      
      # Select variable for color
      selectInput(inputId = "x", 
                  label = "Symbol:",
                  choices = c("Vans" = "VFC", 
                              "Nike" = "NKE", 
                              "Icon Brands" = "ICON", 
                              "Zumiez" = "ZUMZ",
                              "SQBG" = "SQBG",
                              "Adidas" = "ADS"
                  ),
                  selected = "Adidas")
    ),
    
    # Output
    mainPanel(
      plotOutput(outputId = "scatterplot"),
      plotOutput(outputId = "density")
    )
  )
)

# Define server function required to create the scatterplot
server <- function(input, output) {
  skatedat <- reactive({
    req(input$x)
    skate %>% filter(symbol == input$x)
  })
  
  # Create the scatterplot object the plotOutput function is expecting
  output$scatterplot <- renderPlot({
  ggplot(data = skatedat(), aes(x = date, y =close)) +
      geom_line()+
      geom_smooth()+
      scale_x_date()+
      labs(title="Historical Stock Prices")
  })
  output$density <- renderPlot({
    ggplot(data = skatedat(), aes(x = date, y=volume)) +
      geom_line()+
      scale_x_date()+
      labs(title= "Historical Trade Volume")
  })
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)