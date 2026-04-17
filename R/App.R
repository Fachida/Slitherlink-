# app.R - Slitherlink

library(shiny)
source("R/grid.R")
source("R/validation.R")

ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
      body {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
      }
      .title-panel {
        text-align: center;
        color: white;
        padding: 20px;
        margin-bottom: 20px;
      }
      .title-panel h1 {
        font-size: 48px;
        text-shadow: 2px 2px 4px rgba(0,0,0,0.2);
      }
      .well {
        background: white;
        border-radius: 15px;
        padding: 20px;
        box-shadow: 0 4px 6px rgba(0,0,0,0.1);
      }
      .btn-verifier {
        background: linear-gradient(45deg, #28a745, #20c997);
        border: none;
        color: white;
        font-weight: bold;
        width: 100%;
        margin-bottom: 10px;
      }
      .btn-reset {
        background: linear-gradient(45deg, #dc3545, #fd7e14);
        border: none;
        color: white;
        font-weight: bold;
        width: 100%;
      }
      .statut-panel {
        background: white;
        border-radius: 10px;
        padding: 15px;
        margin-top: 20px;
        text-align: center;
        font-weight: bold;
        color: #333;
      }
      .manuel {
        background: #f8f9fa;
        border-radius: 10px;
        padding: 15px;
        margin-top: 20px;
        font-size: 14px;
      }
      .score {
        background: white;
        border-radius: 10px;
        padding: 10px;
        margin-top: 15px;
        text-align: center;
        font-weight: bold;
        color: #28a745;
      }
    "))
  ),
  
  div(class = "title-panel",
      h1("BIENVENUE SUR SLITHERLINK"),
      p("Tracez une boucle fermée en respectant les chiffres")
  ),
  
  sidebarLayout(
    
    sidebarPanel(
      div(class = "well",
          h4(" "),
          selectInput("niveau", "Choisissez votre niveau :",
                      choices = c("Facile (5x5)" = "facile",
                                  "Moyen (7x7)" = "moyen",
                                  "Difficile (9x9)" = "difficile"),
                      selected = "facile"),
          br(),
          actionButton("verifier", "VÉRIFIER", class = "btn-verifier"),
          actionButton("reset", "RECOMMENCER", class = "btn-reset")
      ),
      
      div(class = "score",
          h4("🏆 Meilleur score"),
          textOutput("meilleur_score")
      )
    ),
    
    mainPanel(
      div(style = "text-align: center;",
          plotOutput("plot", click = "click", height = "550px", width = "550px")
      ),
      
      div(class = "statut-panel",
          textOutput("info")
      ),
      
    )
  )
)

server <- function(input, output, session) {
  
  meilleur <- reactiveVal(Inf)
  
  current_grille <- reactiveVal()
  current_taille <- reactiveVal()
  segments_h <- reactiveVal()
  segments_v <- reactiveVal()
  erreurs <- reactiveVal()
  msg <- reactiveVal("")
  
  init_partie <- function(niveau) {
    grille <- get_grille(niveau)
    taille <- get_taille(niveau)
    current_grille(grille)
    current_taille(taille)
    segments_h(matrix(FALSE, nrow = taille + 1, ncol = taille))
    segments_v(matrix(FALSE, nrow = taille, ncol = taille + 1))
    erreurs(matrix(FALSE, nrow = taille, ncol = taille))
    msg("")
  }
  
  observeEvent(input$niveau, {
    init_partie(input$niveau)
  })
  
  observeEvent(input$reset, {
    taille <- current_taille()
    segments_h(matrix(FALSE, nrow = taille + 1, ncol = taille))
    segments_v(matrix(FALSE, nrow = taille, ncol = taille + 1))
    erreurs(matrix(FALSE, nrow = taille, ncol = taille))
    msg("")
  })
  
  observeEvent(input$verifier, {
    grille <- current_grille()
    h <- segments_h()
    v <- segments_v()
    
    err <- detecter_erreurs(grille, h, v)
    erreurs(err)
    total <- sum(h) + sum(v)
    
    if (verifier_regles(grille, h, v)) {
      if (total < meilleur()) meilleur(total)
      msg(paste("🎉 VICTOIRE ! Niveau", input$niveau, "réussi ! 🎉"))
    } else {
      msg("cases rouges = erreurs. Vérifiez les chiffres !")
    }
  })
  
  detecter_segment <- function(x, y) {
    taille <- current_taille()
    n_points <- taille + 1
    seuil <- 0.35
    
    for (l in 1:n_points) {
      for (c in 1:taille) {
        mx <- c + 0.5
        my <- l
        if (sqrt((x - mx)^2 + (y - my)^2) < seuil) {
          return(list(type = "h", ligne = l, colonne = c))
        }
      }
    }
    
    for (l in 1:taille) {
      for (c in 1:n_points) {
        mx <- c
        my <- l + 0.5
        if (sqrt((x - mx)^2 + (y - my)^2) < seuil) {
          return(list(type = "v", ligne = l, colonne = c))
        }
      }
    }
    return(NULL)
  }
  
  observeEvent(input$click, {
    req(current_taille())
    
    seg <- detecter_segment(input$click$x, input$click$y)
    
    if (!is.null(seg)) {
      if (seg$type == "h") {
        mat <- segments_h()
        mat[seg$ligne, seg$colonne] <- !mat[seg$ligne, seg$colonne]
        segments_h(mat)
      } else {
        mat <- segments_v()
        mat[seg$ligne, seg$colonne] <- !mat[seg$ligne, seg$colonne]
        segments_v(mat)
      }
      erreurs(matrix(FALSE, nrow = current_taille(), ncol = current_taille()))
      msg("")
    }
  })
  
  output$meilleur_score <- renderText({
    if (is.infinite(meilleur())) "Aucun score" else paste(meilleur(), "segment(s)")
  })
  
  output$plot <- renderPlot({
    req(current_taille())
    
    grille <- current_grille()
    h <- segments_h()
    v <- segments_v()
    err <- erreurs()
    taille <- current_taille()
    n_points <- taille + 1
    
    par(mar = c(0,0,0,0), bg = "white")
    plot(1:n_points, 1:n_points, type = "n", axes = FALSE, asp = 1,
         xlim = c(0.5, n_points + 0.5), ylim = c(0.5, n_points + 0.5))
    
    for (i in 1:n_points) {
      for (j in 1:n_points) {
        points(i, j, pch = 19, cex = 1.2, col = "#666")
      }
    }
    
    for (l in 1:n_points) {
      for (c in 1:taille) {
        if (h[l, c]) segments(c, l, c + 1, l, lwd = 4, col = "#3498db")
      }
    }
    
    for (l in 1:taille) {
      for (c in 1:n_points) {
        if (v[l, c]) segments(c, l, c, l + 1, lwd = 4, col = "#3498db")
      }
    }
    
    for (i in 1:taille) {
      for (j in 1:taille) {
        val <- grille[i, j]
        if (!is.na(val)) {
          if (err[i, j]) {
            rect(j, i, j + 1, i + 1, col = rgb(1, 0.6, 0.6, alpha = 0.6), border = NA)
          }
          text(j + 0.5, i + 0.5, val, cex = 2.8, font = 2, col = "#2c3e50")
        }
      }
    }
  })
  
  output$info <- renderText({
    total <- sum(segments_h()) + sum(segments_v())
    paste(msg(), "| Segments :", total)
  })
  
  init_partie("facile")
}

shinyApp(ui, server)


