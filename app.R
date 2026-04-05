# =======================================
library(shiny)
source("R/grid.R")

ui <- fluidPage(
  titlePanel("Slitherlink"),
  plotOutput("grille_plot", click = "clic_grille", height = "600px", width = "600px"),
  textOutput("statut"),
  actionButton("reset", "Recommencer", class = "btn-primary")
)

server <- function(input, output, session) {
  
  # Grille de jeu
  ma_grille <- reactiveVal()
  
  observe({
    grille_vide <- creer_grille(taille = 5)
    chiffres <- exemple_grille()
    grille_avec_chiffres <- definir_chiffres(grille_vide, chiffres)
    ma_grille(grille_avec_chiffres)
  })
  
  # Stockage des segments
  segments_h <- reactiveVal(matrix(FALSE, nrow = 6, ncol = 5))
  segments_v <- reactiveVal(matrix(FALSE, nrow = 5, ncol = 6))
  
  # Détection du segment cliqué (VERSION CORRIGÉE)
  detecter_segment <- function(x, y, taille = 5) {
    n_points <- taille + 1
    seuil <- 0.35
    
    # CORRECTION : On garde y tel quel car RStudio donne des coordonnées correctes
    # Mais il faut ajuster car R plot utilise un système différent
    
    # Test pour les segments horizontaux
    for (ligne in 1:n_points) {
      for (colonne in 1:taille) {
        milieu_x <- colonne + 0.5
        milieu_y <- ligne
        dist <- sqrt((x - milieu_x)^2 + (y - milieu_y)^2)
        if (dist < seuil) {
          return(list(type = "h", ligne = ligne, colonne = colonne))
        }
      }
    }
    
    # Test pour les segments verticaux
    for (ligne in 1:taille) {
      for (colonne in 1:n_points) {
        milieu_x <- colonne
        milieu_y <- ligne + 0.5
        dist <- sqrt((x - milieu_x)^2 + (y - milieu_y)^2)
        if (dist < seuil) {
          return(list(type = "v", ligne = ligne, colonne = colonne))
        }
      }
    }
    
    return(NULL)
  }
  
  # Gestion des clics
  observeEvent(input$clic_grille, {
    x <- input$clic_grille$x
    y <- input$clic_grille$y
    
    segment <- detecter_segment(x, y)
    
    if (!is.null(segment)) {
      if (segment$type == "h") {
        mat <- segments_h()
        mat[segment$ligne, segment$colonne] <- !mat[segment$ligne, segment$colonne]
        segments_h(mat)
      } else {
        mat <- segments_v()
        mat[segment$ligne, segment$colonne] <- !mat[segment$ligne, segment$colonne]
        segments_v(mat)
      }
    }
  })
  
  # Bouton reset
  observeEvent(input$reset, {
    segments_h(matrix(FALSE, nrow = 6, ncol = 5))
    segments_v(matrix(FALSE, nrow = 5, ncol = 6))
  })
  
  # Affichage
  output$grille_plot <- renderPlot({
    grille <- ma_grille()
    taille <- nrow(grille)
    n_points <- taille + 1
    
    # Configuration du graphique
    par(mar = c(1, 1, 2, 1))
    plot(1:n_points, 1:n_points, 
         type = "n", axes = FALSE, xlab = "", ylab = "", asp = 1,
         xlim = c(0.5, n_points + 0.5), ylim = c(0.5, n_points + 0.5))
    
    # Points
    for (x in 1:n_points) {
      for (y in 1:n_points) {
        points(x, y, pch = 19, cex = 1.2, col = "black")
      }
    }
    
    # Segments horizontaux
    h_mat <- segments_h()
    for (ligne in 1:n_points) {
      for (colonne in 1:taille) {
        if (h_mat[ligne, colonne]) {
          segments(colonne, ligne, colonne + 1, ligne, lwd = 3, col = "blue")
        }
      }
    }
    
    # Segments verticaux
    v_mat <- segments_v()
    for (ligne in 1:taille) {
      for (colonne in 1:n_points) {
        if (v_mat[ligne, colonne]) {
          segments(colonne, ligne, colonne, ligne + 1, lwd = 3, col = "blue")
        }
      }
    }
    
    # Chiffres
    for (i in 1:taille) {
      for (j in 1:taille) {
        chiffre <- grille[i, j]
        if (!is.na(chiffre)) {
          x_centre <- j + 0.5
          y_centre <- i + 0.5  # CORRECTION : plus d'inversion d'axe
          text(x_centre, y_centre, label = chiffre, cex = 2.5, font = 2, col = "darkred")
        }
      }
    }
    
    title("Slitherlink - Cliquez sur les traits pour tracer")
  })
  
  # Statut
  output$statut <- renderText({
    total_h <- sum(segments_h())
    total_v <- sum(segments_v())
    paste("Segments tracés :", total_h + total_v)
  })
}

shinyApp(ui, server)