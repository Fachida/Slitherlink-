# ui.R - Interface utilisateur et design

library(shiny)

confetti_js <- "
Shiny.addCustomMessageHandler('lancer_confettis', function(message) {
  confetti({ particleCount: 300, spread: 100, origin: { y: 0.6 } });
  setTimeout(function() {
    confetti({ particleCount: 200, spread: 120, origin: { y: 0.7, x: 0.2 } });
    confetti({ particleCount: 200, spread: 120, origin: { y: 0.7, x: 0.8 } });
  }, 200);
});
"

ui <- fluidPage(
  tags$head(
    tags$script(src = "https://cdn.jsdelivr.net/npm/canvas-confetti@1"),
    tags$script(HTML(confetti_js)),
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
        background: linear-gradient(135deg, #60a5fa, #a78bfa);
        -webkit-background-clip: text;
        background-clip: text;
        color: transparent;
        font-size: 52px;
        font-weight: bold;
      }
      .title-panel p {
        color: #94a3b8;
        font-size: 16px;
      }
      .btn-verifier {
        background: linear-gradient(45deg, #10b981, #34d399);
        border: none;
        color: white;
        font-weight: bold;
        padding: 12px 28px;
        border-radius: 40px;
        transition: all 0.3s ease;
        margin: 0 10px;
      }
      .btn-verifier:hover {
        transform: scale(1.05);
        box-shadow: 0 0 15px rgba(16,185,129,0.5);
      }
      .btn-reset {
        background: linear-gradient(45deg, #ef4444, #f97316);
        border: none;
        color: white;
        font-weight: bold;
        padding: 12px 28px;
        border-radius: 40px;
        transition: all 0.3s ease;
        margin: 0 10px;
      }
      .btn-reset:hover {
        transform: scale(1.05);
        box-shadow: 0 0 15px rgba(239,68,68,0.5);
      }
      .statut-panel {
        background: rgba(15, 23, 42, 0.8);
        backdrop-filter: blur(10px);
        padding: 15px;
        border-radius: 20px;
        text-align: center;
        font-size: 18px;
        font-weight: bold;
        margin-top: 20px;
        color: #cbd5e1;
        border: 1px solid #334155;
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
