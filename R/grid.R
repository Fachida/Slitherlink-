# R/grid.R - Fonctions pour gérer la grille

creer_grille <- function(taille = 5) {
  matrix(NA, nrow = taille, ncol = taille)
}

definir_chiffres <- function(grille, chiffres) {
  for (i in 1:nrow(chiffres)) {
    for (j in 1:ncol(chiffres)) {
      if (!is.na(chiffres[i, j])) {
        grille[i, j] <- chiffres[i, j]
      }
    }
  }
  return(grille)
}

exemple_grille <- function() {
  chiffres <- matrix(
    c(NA, 2, NA, 3, NA,
      1, NA, 2, NA, 1,
      NA, 3, NA, 2, NA,
      2, NA, 1, NA, 3,
      NA, 0, NA, 2, NA),
    nrow = 5,
    byrow = TRUE
  )
  return(chiffres)
}

