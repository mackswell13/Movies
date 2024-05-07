# Load required libraries
library(shiny)
library(dplyr)
library(ggplot2)
library(kableExtra)

filtered_movies <- read.csv("filtered_movies.csv")

# Define UI
ui <- fluidPage(
  titlePanel("Movie Director Ratings and Budgets"),
  
  sidebarLayout(
    sidebarPanel(
      selectizeInput("director", "Select Director:", choices = unique(filtered_movies$Director), multiple = TRUE)
    ),
    
    mainPanel(
      plotOutput("scatter_plot"),
      tableOutput("top_movies")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  output$top_movies <- renderTable({
    selected_directors <- input$director
    filtered_movies_by_dir <- filtered_movies %>%
      filter(Director %in% selected_directors) %>%
      mutate(`Budget (Millions)` = Budget / 1000000) %>%
      mutate(`Advised Audience` = Advised.Audience) %>%
      select(Movie, Rating, `Budget (Millions)`, `Advised Audience`, Director) %>%
      arrange(desc(Rating))
    
    return(filtered_movies_by_dir)
  })
  
  
  
  
  
  
  
  output$scatter_plot <- renderPlot({
    selected_directors <- input$director
    filtered_movies_selected <- filtered_movies %>%
      filter(Director %in% selected_directors) %>%
      mutate(`Advised Audience` = Advised.Audience )
    
    ggplot(filtered_movies_selected, aes(x = Budget/1000000, y = Rating, color = Director, shape = `Advised Audience`, label = Movie)) +
      geom_point() +
      geom_text(hjust = 0, vjust = 0, size = 3) +  # Adjust the position and size of the text
      labs(x = "Budget (Millions)", y = "Rating", title = "Movie Budget vs Rating")
  })
  
  
}

# Run the application
shinyApp(ui = ui, server = server)


