# Slitherlink
 
Application Shiny interactive pour jouer au puzzle logique **Slitherlink**, développée dans le cadre de l'UE de programmation R en M1 Statistique et Science des Données.
 
---
 
## Règles du jeu
 
Le Slitherlink se joue sur une grille de points. L'objectif est de relier des points adjacents (horizontalement ou verticalement) pour former **une seule boucle fermée** qui respecte les contraintes suivantes :
 
- La boucle ne doit **pas se croiser** : chaque point a exactement 0 ou 2 segments.
- Les **chiffres dans les cases** indiquent combien de côtés de cette case font partie de la boucle (0, 1, 2 ou 3).
- Les cases sans chiffre n'ont pas de contrainte.
---
 
## Structure du projet
 
```
Slitherlink/
├── DESCRIPTION       # Métadonnées du package R
├── NAMESPACE         # Exports du package
├── app.R             # Application Shiny
└── R/
    ├── grid.R        # Génération aléatoire des puzzles
    ├── validation.R  # Vérification des règles
    └── solver.R      # Solveur ILP (lpSolve)
```
 
---
 
## La bibliothèque R
 
### `grid.R` — Génération des puzzles
 
| Fonction | Description |
|---|---|
| `generer_grille(taille, niveau)` | Génère un puzzle valide par marche aléatoire + frontière XOR |
| `get_grille(niveau)` | Retourne une grille aléatoire pour le niveau donné |
| `get_taille(niveau)` | Retourne la taille : 5 (facile), 6 (moyen), 7 (difficile) |
| `creer_grille(taille)` | Crée une grille vide (NA) |
| `initialiser_segments(taille)` | Initialise les matrices de segments à FALSE |
 
### `validation.R` — Vérification des règles
 
| Fonction | Description |
|---|---|
| `compter_segments_autour(i, j, h, v)` | Nombre de segments actifs autour de la case (i,j) |
| `verifier_regles(grille, h, v)` | Vérifie les contraintes numériques |
| `degre_point(r, c, h, v)` | Degré d'un point (0 à 4) |
| `verifier_degres(h, v)` | Vérifie que tous les points ont degré 0 ou 2 |
| `verifier_boucle_unique(h, v)` | Vérifie la connexité par BFS |
| `verifier_solution_complete(grille, h, v)` | Validation complète en 3 étapes |
 
### `solver.R` — Solveur automatique
 
| Fonction | Description |
|---|---|
| `resoudre(grille, timeout_sec)` | Résout le puzzle par ILP avec élimination de sous-boucles |
 
---
 
## Algorithmes
 
### Génération des puzzles (`generer_grille`)
 
1. **Marche aléatoire fine** dans la grille de cases pour créer une région intérieure sinueuse
2. **Frontière XOR** : un segment est actif si exactement une des deux cases adjacentes est intérieure → garantit une boucle unique valide
3. **Calcul des chiffres** à partir des segments actifs
4. **Masquage équilibré** par quadrants selon le niveau de difficulté
### Solveur ILP (`resoudre`)
 
Formulation du problème comme un programme linéaire en nombres entiers :
 
- **Variables** : $x_s \in \{0,1\}$ (segments), $y_p \in \{0,1\}$ (points)
- **Contraintes cases** : $\sum x_s = \text{chiffre}$ autour de chaque case numérotée
- **Contraintes degrés** : $\sum x_s = 2 y_p$ en chaque point
- **Élimination des sous-boucles** : coupes de Benders ajoutées itérativement jusqu'à obtenir une seule boucle connexe
---
 
## Installation et lancement
 
```r
# Installer les dépendances
install.packages(c("shiny", "lpSolve"))
 
# Lancer l'application
shiny::runApp("app.R")
```
 
---
 
## Fonctionnalités de l'application
 
- **3 niveaux** : Facile (5×5), Moyen (6×6), Difficile (7×7)
- **Génération aléatoire** : nouveau puzzle à chaque partie
- **Interaction** : clic pour tracer/effacer un segment
- **Vérification** : bouton VÉRIFIER pour valider sa solution
- **Solveur automatique** : bouton RÉSOUDRE pour afficher la solution
- **Meilleur score** : nombre de segments de la meilleure solution trouvée
 
