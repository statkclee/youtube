library(shiny)
library(ggplot2)
library(dplyr)

# Assuming comments_nouns_wc is loaded in the global environment
# Load your data here
comments_nouns_wc <- read_rds(str_glue("{here::here()}/data/comments_nouns_wc.rds"))

# UI
ui <- fluidPage(
  title = "Interactive Bar Plot",
  
  # Video ID filter
  selectInput("videoId", "Select Video ID", choices = unique(comments_nouns_wc$videoId)),
  
  # Number of words filter
  sliderInput("nrow", "Number of Words", min = 1, max = 50, value = 10, step = 1),
  
  # Bar plot output
  plotOutput("barplot")
)

# Server
server <- function(input, output, session) {
  # Render the bar plot
  output$barplot <- renderPlot({
    word_counts <- comments_nouns_wc %>%
      filter(videoId == input$videoId) %>%
      pull(wcdata) %>%
      .[[1]] %>%
      slice_head(n = input$nrow)
    
    ggplot(word_counts, aes(x = reorder(noun, n), y = n)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      coord_flip() +
      labs(x = "단어", y = "빈도수") +
      theme_minimal() +
      theme(axis.text.y = element_text(size = 12),
            axis.title = element_text(size = 14),
            plot.title = element_text(size = 16, hjust = 0.5))
  })
}

# Create a Shiny app object
shinyApp(ui, server)