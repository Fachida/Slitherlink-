library(shiny)
source("R/grid.R")
source("R/validation.R")

# ============================================
# SCRIPT POUR CONFETTIS (à mettre au tout début)
# ============================================
jscode <- '
Shiny.addCustomMessageHandler("runjs", function(code) {
  eval(code);
});
'

# ============================================
# INTERFACE UTILISATEUR
# ============================================
ui <- fluidPage(
  tags$head(
    tags$script(src = "https://cdn.jsdelivr.net/npm/canvas-confetti@1"),
    tags$script(HTML(jscode)),
    tags$style(HTML("
      body {
        background: linear-gradient(135deg, #0f172a 0%, #1e293b 100%);
        font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
      }
      .title-panel {
        text-align: center;
        margin-bottom: 30px;
      }
      .title-panel h1 {
        color: #60a5fa;
        font-size: 48px;
        font-weight: bold;
      }
      .title-panel p {
        color: #94a3b8;
        font-size: 16px;
      }
      .btn-verifier {
        background: #10b981;
        border: none;
        color: white;
        font-weight: bold;
        padding: 12px 24px;
        border-radius: 30px;
        margin: 0 10px;
      }
      .btn-verifier:hover {
        background: #34d399;
      }
      .btn-reset {
        background: #ef4444;
        border: none;
        color: white;
        font-weight: bold;
        padding: 12px 24px;
        border-radius: 30px;
        margin: 0 10px;
      }
      .btn-reset:hover {
        background: #f97316;
      }
      .statut-panel {
        background: rgba(0,0,0,0.5);
        padding: 15px;
        border-radius: 15px;
        text-align: center;
        font-size: 18px;
        font-weight: bold;
        margin-top: 20px;
        color: #cbd5e1;
      }
    "))
  ),
  
  div(class = "title-panel",
      h1("🐍 SLITHERLINK"),
      p("Tracez la boucle fermée en respectant les chiffres")
  ),
  
  div(style = "display: flex; justify-content: center;",
      plotOutput("grille_plot", click = "clic_grille", height = "600px", width = "600px")
  ),
  
  div(style = "display: flex; justify-content: center; gap: 20px; margin-top: 20px;",
      actionButton("verifier", "✓ VÉRIFIER", class = "btn-verifier"),
      actionButton("reset", "⟳ RECOMMENCER", class = "btn-reset")
  ),
  
  div(class = "statut-panel",
      textOutput("statut")
  )
)

# ============================================
# SERVEUR (LOGIQUE)
# ============================================
server <- function(input, output, session) {
  
  # Fonction pour exécuter du JavaScript (définie ici, après session)
  runjs <- function(code) {
    session$sendCustomMessage("runjs", code)
  }
  
  ma_grille <- reactiveVal()
  erreurs <- reactiveVal(matrix(FALSE, nrow = 5, ncol = 5))
  
  observe({
    grille_vide <- creer_grille(taille = 5)
    chiffres <- exemple_grille()
    grille_avec_chiffres <- definir_chiffres(grille_vide, chiffres)
    ma_grille(grille_avec_chiffres)
  })
  
  segments_h <- reactiveVal(matrix(FALSE, nrow = 6, ncol = 5))
  segments_v <- reactiveVal(matrix(FALSE, nrow = 5, ncol = 6))
  verification_message <- reactiveVal("")
  
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
  
  observeEvent(input$verifier, {
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
      verification_message("🎉 FÉLICITATIONS ! VICTOIRE ! 🎉")
      # Confettis qui marchent maintenant !
      runjs("canvasConfetti({ particleCount: 300, spread: 100, origin: { y: 0.6 } });")
      # Deuxième salve
      runjs("setTimeout(function() { canvasConfetti({ particleCount: 200, spread: 120, origin: { y: 0.7, x: 0.2 } }); }, 200);")
      runjs("setTimeout(function() { canvasConfetti({ particleCount: 200, spread: 120, origin: { y: 0.7, x: 0.8 } }); }, 200);")
    } else {
      verification_message("❌ Cases rouges = erreurs. Continuez ! ❌")
    }
  })
  
  observeEvent(input$reset, {
    segments_h(matrix(FALSE, nrow = 6, ncol = 5))
    segments_v(matrix(FALSE, nrow = 5, ncol = 6))
    verification_message("")
    erreurs(matrix(FALSE, nrow = 5, ncol = 5))
  })
  
  output$grille_plot <- renderPlot({
    grille <- ma_grille()
    erreurs_mat <- erreurs()
    taille <- nrow(grille)
    n_points <- taille + 1
    
    par(mar = c(1, 1, 2, 1), bg = "#0f172a")
    plot(1:n_points, 1:n_points, 
         type = "n", axes = FALSE, xlab = "", ylab = "", asp = 1,
         xlim = c(0.5, n_points + 0.5), ylim = c(0.5, n_points + 0.5))
    
    # Grille de fond
    for (i in 1:n_points) {
      for (j in 1:n_points) {
        rect(j - 0.5, i - 0.5, j + 0.5, i + 0.5, 
             border = "#334155", lwd = 0.5)
      }
    }
    
    # Points
    for (x in 1:n_points) {
      for (y in 1:n_points) {
        points(x, y, pch = 19, cex = 1.3, col = "#94a3b8")
      }
    }
    
    # Segments horizontaux
    h_mat <- segments_h()
    for (ligne in 1:n_points) {
      for (colonne in 1:taille) {
        if (h_mat[ligne, colonne]) {
          segments(colonne, ligne, colonne + 1, ligne, lwd = 4, col = "#60a5fa")
        }
      }
    }
    
    # Segments verticaux
    v_mat <- segments_v()
    for (ligne in 1:taille) {
      for (colonne in 1:n_points) {
        if (v_mat[ligne, colonne]) {
          segments(colonne, ligne, colonne, ligne + 1, lwd = 4, col = "#60a5fa")
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
            rect(j, i, j + 1, i + 1, col = rgb(1, 0.3, 0.3, alpha = 0.5), border = NA)
          }
          
          text(x_centre, y_centre, label = chiffre, cex = 3, font = 2, col = "#fbbf24")
        }
      }
    }
    
    title("Cliquez sur les segments pour tracer", col.main = "#94a3b8", cex.main = 1.2)
  })
  
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

# ============================================
# LANCER L'APPLICATION
# ============================================
shinyApp(ui, server)