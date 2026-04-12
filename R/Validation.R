# R/validation.R - Validation des règles

# Compter les segments autour d'une case
compter_segments_autour <- function(i, j, segments_h, segments_v) {
  somme <- 0
  if (segments_h[i, j]) somme <- somme + 1
  if (segments_h[i + 1, j]) somme <- somme + 1
  if (segments_v[i, j]) somme <- somme + 1
  if (segments_v[i, j + 1]) somme <- somme + 1
  return(somme)
}

# Vérifier si toutes les cases respectent leur chiffre
verifier_regles <- function(grille, segments_h, segments_v) {
  taille <- nrow(grille)
  for (i in 1:taille) {
    for (j in 1:taille) {
      chiffre <- grille[i, j]
      if (!is.na(chiffre)) {
        nb <- compter_segments_autour(i, j, segments_h, segments_v)
        if (nb != chiffre) {
          return(FALSE)
        }
      }
    }
  }
  return(TRUE)
}

# Détecter les cases en erreur
detecter_erreurs <- function(grille, segments_h, segments_v) {
  taille <- nrow(grille)
  erreurs <- matrix(FALSE, nrow = taille, ncol = taille)
  for (i in 1:taille) {
    for (j in 1:taille) {
      chiffre <- grille[i, j]
      if (!is.na(chiffre)) {
        nb <- compter_segments_autour(i, j, segments_h, segments_v)
        if (nb != chiffre) {
          erreurs[i, j] <- TRUE
        }
      }
    }
  }
  return(erreurs)
}