
library(shiny)
library(ggplot2)
library(dplyr)

ui <- fluidPage(
  plotOutput("scatter", brush = "plot_brush"),
  textInput("label", "Label"),
  actionButton("label_button", "Label Points"),
  downloadButton("download_data", "Download Data"),
  tableOutput("labeled_data")
)

server <- function(input, output, session) {
  dataset2 <- read.csv("data/dataset3.csv")
  dataset2a = read.csv("data/activity2/dataset3a.csv")
  dataset2=dataset2 %>% merge(dataset3a, all=T) %>% filter(is.na(label))
  rv <- reactiveValues(
    labeled_data = tibble(x = numeric(), y = numeric(), label = character())
  )
  
  output$scatter <- renderPlot({
    ggplot(dataset2, aes(x, y)) +
      geom_point() +
      geom_text(data = rv$labeled_data, aes(x = x, y = y, label = label), vjust = 0, nudge_y = 1)
  })
  
  observeEvent(input$label_button, {
    brushed_points <- brushedPoints(dataset2, input$plot_brush)
    
    if (nrow(brushed_points) > 0) {
      new_labels <- tibble(x = brushed_points$x, y = brushed_points$y, label = input$label)
      rv$labeled_data <- rv$labeled_data %>% bind_rows(new_labels)
    }
  })
  
  output$download_data <- downloadHandler(
    filename = function() {
      "labeled_data.csv"
    },
    content = function(file) {
      write.csv(rv$labeled_data, file, row.names = FALSE)
    }
  )
  
  output$labeled_data <- renderTable({
    rv$labeled_data
  })
}

shinyApp(ui, server)

