# interface.R - Interface utilisateur (style cours Shiny)

library(shiny)

ui <- fluidPage(
  
  # CSS personnalisé (comme montré dans le cours)
  tags$head(
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
  
  # Layout comme dans le cours : sidebar + main
  sidebarLayout(
    
    # Panneau latéral (widgets)
    sidebarPanel(
      h4("🎮 Paramètres"),
      
      # Sélecteur de niveau (comme selectInput dans le cours)
      selectInput("niveau", 
                  label = "Choisis ton niveau :",
                  choices = c("🍃 Facile (5x5)" = "facile",
                              "⚡ Moyen (7x7)" = "moyen",
                              "🔥 Difficile (9x9)" = "difficile"),
                  selected = "facile"),
      
      hr(),  # Ligne séparatrice
      
      h4("🎯 Actions"),
      
      # Boutons comme dans le cours
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
    
    # Panneau principal (affichage de la grille)
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
