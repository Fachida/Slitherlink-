# app.R - Application Shiny

library(shiny)
source("R/grid.R")

ui <- fluidPage(
  titlePanel("Slitherlink"),
  plotOutput("grille_plot", click = "clic_grille"),
  textOutput("statut")
)

server <- function(input, output) {
  
  ma_grille <- reactiveVal()
  
  observe({
    grille_vide <- creer_grille(taille = 5)
    chiffres <- exemple_grille()
    grille_avec_chiffres <- definir_chiffres(grille_vide, chiffres)
    ma_grille(grille_avec_chiffres)
  })
  
  output$grille_plot <- renderPlot({
    grille <- ma_grille()
    taille <- nrow(grille)
    n_points <- taille + 1
    
    plot(1:n_points, 1:n_points, 
         type = "n", axes = FALSE, xlab = "", ylab = "", asp = 1,
         xlim = c(0.5, n_points + 0.5), ylim = c(0.5, n_points + 0.5))
    
    # Points
    for (x in 1:n_points) {
      for (y in 1:n_points) {
        points(x, y, pch = 19, cex = 1.5, col = "black")
      }
    }
    
    # Chiffres
    for (i in 1:taille) {
      for (j in 1:taille) {
        chiffre <- grille[i, j]
        if (!is.na(chiffre)) {
          x_centre <- j + 0.5
          y_centre <- (taille - i + 1) + 0.5
          text(x_centre, y_centre, label = chiffre, cex = 2, font = 2)
        }
      }
    }
    
    title("Cliquez pour tracer la boucle")
  })
  
  output$statut <- renderText({
    req(input$clic_grille)
    paste("Clic à : x =", round(input$clic_grille$x, 2), 
          ", y =", round(input$clic_grille$y, 2))
  })
}

shinyApp(ui, server)