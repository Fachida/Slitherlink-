# app.R - Application principale

library(shiny)
source("R/grid.R")
source("R/validation.R")
source("R/grilles.R")

# ============================================
# INTERFACE UTILISATEUR
# ============================================

ui <- fluidPage(
  
  # CSS personnalisé
  tags$head(
    tags$script(src = "https://cdn.jsdelivr.net/npm/canvas-confetti@1"),
    tags$style(HTML("
      body {
        background-color: #f8f9fa;
        font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
      }
      .title-panel {
        text-align: center;
        background-color: #2c3e50;
        color: white;
        padding: 15px;
        margin-bottom: 20px;
        border-radius: 8px;
      }
      .well {
        background-color: white;
        border-radius: 8px;
        box-shadow: 0 2px 5px rgba(0,0,0,0.1);
      }
      .btn-primary {
        background-color: #3498db;
        border: none;
        width: 100%;
        margin-bottom: 10px;
      }
      .btn-primary:hover {
        background-color: #2980b9;
      }
      .btn-success {
        background-color: #27ae60;
        border: none;
        width: 100%;
      }
      .btn-success:hover {
        background-color: #2ecc71;
      }
      .btn-danger {
        background-color: #e74c3c;
        border: none;
        width: 100%;
      }
      .statut-panel {
        background-color: #ecf0f1;
        padding: 10px;
        border-radius: 8px;
        text-align: center;
        font-weight: bold;
        margin-top: 15px;
      }
      .aide-text {
        color: #7f8c8d;
        font-size: 12px;
        margin-top: 10px;
        text-align: center;
      }
    "))
  ),
  
  # Titre principal
  div(class = "title-panel",
      h1("🐍 SLITHERLINK"),
      p("Trace la boucle fermée en respectant les chiffres")
  ),
  
  # Layout sidebar + main
  sidebarLayout(
    
    # Panneau latéral
    sidebarPanel(
      h4("🎮 Paramètres"),
      
      selectInput("niveau", 
                  label = "Choisis ton niveau :",
                  choices = c("🍃 Facile (5x5)" = "facile",
                              "⚡ Moyen (7x7)" = "moyen",
                              "🔥 Difficile (9x9)" = "difficile"),
                  selected = "facile"),
      
      hr(),
      
      h4("🎯 Actions"),
      actionButton("verifier", "✓ VÉRIFIER", class = "btn-primary"),
      actionButton("reset", "⟳ RECOMMENCER", class = "btn-danger"),
      
      hr(),
      
      h4("📖 Règles"),
      p("• Un chiffre = nombre de segments autour"),
      p("• 0 = aucun segment, 1 = un, 2 = deux, 3 = trois"),
      p("• ❌ Case rouge = erreur"),
      
      div(class = "aide-text",
          "💡 Clique sur un segment pour le tracer. Reclique pour l'effacer."
      )
    ),
    
    # Panneau principal
    mainPanel(
      plotOutput("grille_plot", 
                 click = "clic_grille",
                 height = "550px",
                 width = "550px"),
      
      div(class = "statut-panel",
          textOutput("statut")
      )
    )
  )
)

# ============================================
# SERVEUR (LOGIQUE)
# ============================================

server <- function(input, output, session) {
  
  # État du jeu
  ma_grille <- reactiveVal()
  erreurs <- reactiveVal()
  verification_message <- reactiveVal("")
  
  # Stockage des segments
  segments_h <- reactiveVal()
  segments_v <- reactiveVal()
  
  # Initialiser une nouvelle partie
  init_partie <- function(niveau) {
    taille <- get_taille(niveau)
    chiffres <- get_grille(niveau)
    
    grille_vide <- creer_grille(taille = taille)
    grille_avec_chiffres <- definir_chiffres(grille_vide, chiffres)
    
    ma_grille(grille_avec_chiffres)
    erreurs(matrix(FALSE, nrow = taille, ncol = taille))
    segments_h(matrix(FALSE, nrow = taille + 1, ncol = taille))
    segments_v(matrix(FALSE, nrow = taille, ncol = taille + 1))
    verification_message("")
  }
  
  # Changer de niveau
  observeEvent(input$niveau, {
    init_partie(input$niveau)
  })
  
  # Détection du segment cliqué
  detecter_segment <- function(x, y, taille) {
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
    req(!is.null(ma_grille()))
    
    grille <- ma_grille()
    taille <- nrow(grille)
    
    segment <- detecter_segment(input$clic_grille$x, input$clic_grille$y, taille)
    
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
      erreurs(matrix(FALSE, nrow = taille, ncol = taille))
    }
  })
  
  # Vérification
  observeEvent(input$verifier, {
    req(!is.null(ma_grille()))
    
    grille <- ma_grille()
    h_mat <- segments_h()
    v_mat <- segments_v()
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
      verification_message(paste("🎉 FÉLICITATIONS ! VICTOIRE au niveau", input$niveau, "! 🎉"))
      # Confettis
      session$sendCustomMessage("confettis", list())
    } else {
      verification_message("❌ Cases rouges = erreurs. Vérifie le nombre de segments autour du chiffre ! ❌")
    }
  })
  
  # Reset
  observeEvent(input$reset, {
    req(!is.null(ma_grille()))
    taille <- nrow(ma_grille())
    segments_h(matrix(FALSE, nrow = taille + 1, ncol = taille))
    segments_v(matrix(FALSE, nrow = taille, ncol = taille + 1))
    verification_message("")
    erreurs(matrix(FALSE, nrow = taille, ncol = taille))
  })
  
  # Affichage de la grille
  output$grille_plot <- renderPlot({
    req(!is.null(ma_grille()), !is.null(segments_h()), !is.null(segments_v()))
    
    grille <- ma_grille()
    erreurs_mat <- erreurs()
    h_mat <- segments_h()
    v_mat <- segments_v()
    
    taille <- nrow(grille)
    n_points <- taille + 1
    
    par(mar = c(1, 1, 2, 1), bg = "white")
    plot(1:n_points, 1:n_points, 
         type = "n", axes = FALSE, xlab = "", ylab = "", asp = 1,
         xlim = c(0.5, n_points + 0.5), ylim = c(0.5, n_points + 0.5))
    
    # Grille de fond
    for (i in 1:n_points) {
      for (j in 1:n_points) {
        rect(j - 0.5, i - 0.5, j + 0.5, i + 0.5, 
             border = "#dee2e6", lwd = 0.8)
      }
    }
    
    # Points
    for (x in 1:n_points) {
      for (y in 1:n_points) {
        points(x, y, pch = 19, cex = 1.2, col = "#adb5bd")
      }
    }
    
    # Segments horizontaux
    for (ligne in 1:n_points) {
      for (colonne in 1:taille) {
        if (!is.null(h_mat) && h_mat[ligne, colonne]) {
          segments(colonne, ligne, colonne + 1, ligne, lwd = 3, col = "#3498db")
        }
      }
    }
    
    # Segments verticaux
    for (ligne in 1:taille) {
      for (colonne in 1:n_points) {
        if (!is.null(v_mat) && v_mat[ligne, colonne]) {
          segments(colonne, ligne, colonne, ligne + 1, lwd = 3, col = "#3498db")
        }
      }
    }
    
    # Chiffres
    for (i in 1:taille) {
      for (j in 1:taille) {
        chiffre <- grille[i, j]
        if (!is.na(chiffre)) {
          x_centre <- j + 0.5
          y_centre <- i + 0.5
          
          if (erreurs_mat[i, j]) {
            rect(j, i, j + 1, i + 1, col = rgb(0.9, 0.3, 0.3, alpha = 0.35), border = NA)
          }
          
          text(x_centre, y_centre, label = chiffre, cex = 2.8, font = 2, col = "#2c3e50")
        }
      }
    }
    
    title(paste("Niveau :", input$niveau), col.main = "#6c757d", cex.main = 1.1)
  })
  
  # Statut
  output$statut <- renderText({
    total_h <- if (!is.null(segments_h())) sum(segments_h()) else 0
    total_v <- if (!is.null(segments_v())) sum(segments_v()) else 0
    msg <- verification_message()
    if (msg != "") {
      paste(msg, "📏 Segments :", total_h + total_v)
    } else {
      paste("📏 Segments tracés :", total_h + total_v)
    }
  })
  
  # Initialisation par défaut
  init_partie("facile")
}

# ============================================
# SCRIPT CONFETTIS
# ============================================

jscode <- '
Shiny.addCustomMessageHandler("confettis", function(message) {
  canvasConfetti({ particleCount: 200, spread: 70, origin: { y: 0.6 } });
  setTimeout(function() {
    canvasConfetti({ particleCount: 150, spread: 100, origin: { y: 0.7, x: 0.2 } });
    canvasConfetti({ particleCount: 150, spread: 100, origin: { y: 0.7, x: 0.8 } });
  }, 200);
});
'

# Ajouter le script à l'interface
ui <- tagList(
  tags$head(tags$script(HTML(jscode))),
  ui
)

# ============================================
# LANCER L'APPLICATION
# ============================================

shinyApp(ui = ui, server = server)