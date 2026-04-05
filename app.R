library(shiny)
source("R/grid.R")
source("R/validation.R")

ui <- fluidPage(
  titlePanel("Slitherlink"),
  plotOutput("grille_plot", click = "clic_grille", height = "700px", width = "700px"),
  fluidRow(
    column(4, textOutput("statut")),
    column(4, actionButton("verifier", "Vérifier", class = "btn-success")),
    column(4, actionButton("reset", "Recommencer", class = "btn-primary"))
  )
)

server <- function(input, output, session) {
  
  # Grille de jeu
  ma_grille <- reactiveVal()
  
  # Grille pour stocker les erreurs (cases en rouge)
  erreurs <- reactiveVal(matrix(FALSE, nrow = 5, ncol = 5))
  
  observe({
    grille_vide <- creer_grille(taille = 5)
    chiffres <- exemple_grille()
    grille_avec_chiffres <- definir_chiffres(grille_vide, chiffres)
    ma_grille(grille_avec_chiffres)
  })
  
  # Stockage des segments
  segments_h <- reactiveVal(matrix(FALSE, nrow = 6, ncol = 5))
  segments_v <- reactiveVal(matrix(FALSE, nrow = 5, ncol = 6))
  
  verification_message <- reactiveVal("")
  
  # Détection du segment cliqué
  detecter_segment <- function(x, y, taille = 5) {
    n_points <- taille + 1
    seuil <- 0.35
    
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
      verification_message("")
      erreurs(matrix(FALSE, nrow = 5, ncol = 5))
    }
  })
  
  # Bouton vérifier avec détection des erreurs
  observeEvent(input$verifier, {
    grille <- ma_grille()
    h_mat <- segments_h()
    v_mat <- segments_v()
    
    # Vérifier chaque case
    taille <- nrow(grille)
    erreurs_mat <- matrix(FALSE, nrow = taille, ncol = taille)
    tout_bon <- TRUE
    
    for (i in 1:taille) {
      for (j in 1:taille) {
        chiffre <- grille[i, j]
        if (!is.na(chiffre)) {
          nb_segments <- compter_segments_autour(i, j, h_mat, v_mat)
          if (nb_segments != chiffre) {
            erreurs_mat[i, j] <- TRUE
            tout_bon <- FALSE
          }
        }
      }
    }
    
    erreurs(erreurs_mat)
    
    if (tout_bon) {
      verification_message("🎉 BRAVO ! La grille est correcte ! 🎉")
    } else {
      verification_message("❌ Cases rouges = erreurs. Vérifie ces cases !")
    }
  })
  
  # Bouton reset
  observeEvent(input$reset, {
    segments_h(matrix(FALSE, nrow = 6, ncol = 5))
    segments_v(matrix(FALSE, nrow = 5, ncol = 6))
    verification_message("")
    erreurs(matrix(FALSE, nrow = 5, ncol = 5))
  })
  
  # Affichage
  output$grille_plot <- renderPlot({
    grille <- ma_grille()
    erreurs_mat <- erreurs()
    taille <- nrow(grille)
    n_points <- taille + 1
    
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
    
    # Chiffres avec fond rouge si erreur
    for (i in 1:taille) {
      for (j in 1:taille) {
        chiffre <- grille[i, j]
        if (!is.na(chiffre)) {
          x_centre <- j + 0.5
          y_centre <- i + 0.5
          
          # Si la case est en erreur, dessiner un fond rouge
          if (erreurs_mat[i, j]) {
            rect(j, i, j + 1, i + 1, col = rgb(1, 0.8, 0.8, alpha = 0.7), border = NA)
          }
          
          text(x_centre, y_centre, label = chiffre, cex = 2.5, font = 2, col = "darkred")
        }
      }
    }
    
    title("Slitherlink - Cases rouges = erreurs")
  })
  
  # Statut
  output$statut <- renderText({
    total_h <- sum(segments_h())
    total_v <- sum(segments_v())
    msg <- verification_message()
    if (msg != "") {
      paste(msg, "| Segments :", total_h + total_v)
    } else {
      paste("Segments tracés :", total_h + total_v)
    }
  })
}

shinyApp(ui, server)