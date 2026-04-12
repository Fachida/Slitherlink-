library(shiny)

ui <- fluidPage(
  plotOutput("grille", height = "500px", width = "500px")
)

server <- function(input, output, session) {
  output$grille <- renderPlot({
    plot(1:6, 1:6, type = "n", axes = FALSE, asp = 1)
    for (i in 1:6) for (j in 1:6) points(i, j, pch = 19)
  })
}

shinyApp(ui, server)