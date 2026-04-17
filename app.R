# app.R - Slitherlink

library(shiny)
source("R/grid.R")
source("R/validation.R")
source("R/solver.R")

ui <- fluidPage(
  
  tags$head(
    
    # Librairie confettis
    tags$script(src = "https://cdn.jsdelivr.net/npm/canvas-confetti@1.6.0/dist/confetti.browser.min.js"),
    
    # Handler confettis + styles
    tags$script(HTML("
      Shiny.addCustomMessageHandler('lancer_confettis', function(msg) {
        var duration = 3000;
        var end = Date.now() + duration;
        (function frame() {
          confetti({
            particleCount: 6,
            angle: 60,
            spread: 55,
            origin: { x: 0 },
            colors: ['#667eea', '#764ba2', '#e83e8c', '#20c997', '#ffd700']
          });
          confetti({
            particleCount: 6,
            angle: 120,
            spread: 55,
            origin: { x: 1 },
            colors: ['#667eea', '#764ba2', '#e83e8c', '#20c997', '#ffd700']
          });
          if (Date.now() < end) requestAnimationFrame(frame);
        }());
      });
    ")),
    
    tags$style(HTML("
      body {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
      }
      .title-panel {
        text-align: center; color: white;
        padding: 20px; margin-bottom: 20px;
      }
      .title-panel h1 {
        font-size: 48px;
        text-shadow: 2px 2px 4px rgba(0,0,0,0.2);
      }
      .well {
        background: white; border-radius: 15px;
        padding: 20px; box-shadow: 0 4px 6px rgba(0,0,0,0.1);
      }
      .btn-verifier {
        background: linear-gradient(45deg, #28a745, #20c997);
        border: none; color: white; font-weight: bold;
        width: 100%; margin-bottom: 10px;
      }
      .btn-resoudre {
        background: linear-gradient(45deg, #6f42c1, #e83e8c);
        border: none; color: white; font-weight: bold;
        width: 100%; margin-bottom: 10px;
      }
      .btn-annuler {
        background: linear-gradient(45deg, #fd7e14, #ffc107);
        border: none; color: white; font-weight: bold;
        width: 100%; margin-bottom: 10px;
      }
      .btn-reset {
        background: linear-gradient(45deg, #dc3545, #fd7e14);
        border: none; color: white; font-weight: bold; width: 100%;
      }
      .statut-panel {
        background: white; border-radius: 10px; padding: 15px;
        margin-top: 20px; text-align: center;
        font-weight: bold; color: #333;
      }
      .score {
        background: white; border-radius: 10px; padding: 10px;
        margin-top: 15px; text-align: center;
        font-weight: bold; color: #28a745;
      }
      .regles {
        background: #f8f9fa; border-radius: 10px;
        padding: 15px; margin-top: 15px;
        font-size: 13px; color: #333;
        border-left: 4px solid #667eea;
      }
      .regles h5 {
        color: #667eea; font-weight: bold; margin-bottom: 8px;
      }
      .regles ul {
        padding-left: 18px; margin-bottom: 0;
      }
      .regles li {
        margin-bottom: 5px; line-height: 1.4;
      }
    "))
  ),
  
  div(class = "title-panel",
      h1("BIENVENUE SUR SLITHERLINK"),
      p("Tracez une boucle fermée en respectant les chiffres")),
  
  sidebarLayout(
    sidebarPanel(
      div(class = "well",
          h4(" "),
          selectInput("niveau", "Choisissez votre niveau :",
                      choices = c("Facile (5x5)"    = "facile",
                                  "Moyen (6x6)"     = "moyen",
                                  "Difficile (7x7)" = "difficile"),
                      selected = "facile"),
          br(),
          actionButton("verifier", "VÉRIFIER",    class = "btn-verifier"),
          actionButton("annuler",  "ANNULER",     class = "btn-annuler"),
          actionButton("resoudre", "RÉSOUDRE",    class = "btn-resoudre"),
          actionButton("reset",    "RECOMMENCER", class = "btn-reset")
      ),
      
      div(class = "score",
          h4("🏆 Meilleur score"),
          textOutput("meilleur_score")),
      
      # Règles du jeu
      div(class = "regles",
          h5("📖 Règles du jeu"),
          tags$ul(
            tags$li("Reliez les points pour former", tags$b("une seule boucle fermée.")),
            tags$li("La boucle ne doit", tags$b("ni se croiser ni se ramifier.")),
            tags$li("Chaque chiffre indique combien de", tags$b("côtés de la case"), "font partie de la boucle."),
            tags$li("Les cases vides n'ont", tags$b("aucune contrainte."))
          )
      )
    ),
    
    mainPanel(
      div(style = "text-align: center;",
          plotOutput("plot", click = "click",
                     height = "550px", width = "550px")),
      div(class = "statut-panel", textOutput("info"))
    )
  )
)

server <- function(input, output, session) {
  
  meilleur       <- reactiveVal(Inf)
  current_grille <- reactiveVal(NULL)
  current_taille <- reactiveVal(5L)
  segments_h     <- reactiveVal(matrix(FALSE, nrow = 6L, ncol = 5L))
  segments_v     <- reactiveVal(matrix(FALSE, nrow = 5L, ncol = 6L))
  msg            <- reactiveVal("")
  
  # Historique pour le bouton ANNULER (pile LIFO)
  historique     <- reactiveVal(list())
  
  # ----------------------------------------------------------
  # Initialisation
  # ----------------------------------------------------------
  init_partie <- function(niveau) {
    taille <- get_taille(niveau)
    segs   <- initialiser_segments(taille)
    grille <- get_grille(niveau)
    current_grille(grille)
    current_taille(taille)
    segments_h(segs$h)
    segments_v(segs$v)
    historique(list())
    msg("")
  }
  
  observeEvent(session$clientData$url_hostname, {
    init_partie("facile")
  }, once = TRUE)
  
  observeEvent(input$niveau, { init_partie(input$niveau) },
               ignoreInit = FALSE)
  
  observeEvent(input$reset, { init_partie(input$niveau) })
  
  # ----------------------------------------------------------
  # Bouton ANNULER — dépile le dernier état
  # ----------------------------------------------------------
  observeEvent(input$annuler, {
    hist <- historique()
    if (length(hist) > 0L) {
      dernier    <- hist[[length(hist)]]
      segments_h(dernier$h)
      segments_v(dernier$v)
      historique(hist[-length(hist)])
      msg("")
    }
  })
  
  # ----------------------------------------------------------
  # Bouton VÉRIFIER
  # ----------------------------------------------------------
  observeEvent(input$verifier, {
    req(current_grille())
    grille <- current_grille()
    h      <- segments_h()
    v      <- segments_v()
    
    if (verifier_solution_complete(grille, h, v)) {
      total <- sum(h) + sum(v)
      if (total < meilleur()) meilleur(total)
      msg(paste("🎉 VICTOIRE ! Niveau", input$niveau, "réussi ! 🎉"))
      session$sendCustomMessage("lancer_confettis", list())
    } else {
      msg("❌ Pas encore... La boucle n'est pas valide.")
    }
  })
  
  # ----------------------------------------------------------
  # Solveur ILP
  # ----------------------------------------------------------
  observeEvent(input$resoudre, {
    req(current_grille())
    withProgress(message = "Résolution en cours...", value = 0.5, {
      res <- resoudre(current_grille(), timeout_sec = 60)
    })
    if (isTRUE(res$resolu)) {
      segments_h(res$segments_h)
      segments_v(res$segments_v)
      historique(list())
      msg("✅ Solution trouvée par le solveur !")
    } else if (!is.null(res$message) && res$message == "timeout") {
      msg("⏱️ Temps dépassé.")
    } else {
      msg("❌ Aucune solution trouvée.")
    }
  })
  
  # ----------------------------------------------------------
  # Détection du segment cliqué (axe Y corrigé)
  # ----------------------------------------------------------
  detecter_segment <- function(x, y) {
    taille   <- current_taille()
    n_points <- taille + 1L
    seuil    <- 0.35
    fy       <- function(r) n_points + 1L - r
    
    for (l in seq_len(n_points))
      for (c in seq_len(taille))
        if (sqrt((x - (c + 0.5))^2 + (y - fy(l))^2) < seuil)
          return(list(type = "h", ligne = l, colonne = c))
    
    for (l in seq_len(taille))
      for (c in seq_len(n_points))
        if (sqrt((x - c)^2 + (y - (fy(l) - 0.5))^2) < seuil)
          return(list(type = "v", ligne = l, colonne = c))
    NULL
  }
  
  # ----------------------------------------------------------
  # Clic : sauvegarde l'état, bascule le segment
  # ----------------------------------------------------------
  observeEvent(input$click, {
    req(current_taille(), current_grille())
    seg <- detecter_segment(input$click$x, input$click$y)
    
    if (!is.null(seg)) {
      
      # Sauvegarder l'état courant dans l'historique
      hist <- historique()
      hist <- c(hist, list(list(h = segments_h(), v = segments_v())))
      historique(hist)
      
      # Basculer le segment
      if (seg$type == "h") {
        mat <- segments_h()
        mat[seg$ligne, seg$colonne] <- !mat[seg$ligne, seg$colonne]
        segments_h(mat)
      } else {
        mat <- segments_v()
        mat[seg$ligne, seg$colonne] <- !mat[seg$ligne, seg$colonne]
        segments_v(mat)
      }
      msg("")
    }
  })
  
  # ----------------------------------------------------------
  # Outputs texte
  # ----------------------------------------------------------
  output$meilleur_score <- renderText({
    if (is.infinite(meilleur())) "Aucun score"
    else paste(meilleur(), "segment(s)")
  })
  
  output$info <- renderText({
    total <- sum(segments_h()) + sum(segments_v())
    paste(msg(), "| Segments :", total)
  })
  
  # ----------------------------------------------------------
  # Rendu graphique
  # ----------------------------------------------------------
  output$plot <- renderPlot({
    req(current_grille())
    
    grille   <- current_grille()
    h        <- segments_h()
    v        <- segments_v()
    taille   <- current_taille()
    n_points <- taille + 1L
    fy       <- function(r) n_points + 1L - r
    
    par(mar = c(0, 0, 0, 0), bg = "white")
    plot(NULL, axes = FALSE, asp = 1,
         xlim = c(0.5, n_points + 0.5),
         ylim = c(0.5, n_points + 0.5))
    
    # Chiffres
    for (i in seq_len(taille))
      for (j in seq_len(taille)) {
        val <- grille[i, j]
        if (!is.na(val))
          text(j + 0.5, fy(i) - 0.5, val,
               cex = 2.8, font = 2, col = "#2c3e50")
      }
    
    # Segments horizontaux
    for (l in seq_len(n_points))
      for (c in seq_len(taille))
        if (h[l, c])
          segments(c, fy(l), c + 1L, fy(l), lwd = 4, col = "#3498db")
    
    # Segments verticaux
    for (l in seq_len(taille))
      for (c in seq_len(n_points))
        if (v[l, c])
          segments(c, fy(l), c, fy(l + 1L), lwd = 4, col = "#3498db")
    
    # Points (par-dessus les segments)
    for (r in seq_len(n_points))
      for (c in seq_len(n_points))
        points(c, fy(r), pch = 19, cex = 1.2, col = "#666")
  })
}

shinyApp(ui, server)