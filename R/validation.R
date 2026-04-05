# R/validation.R - Vérification des règles du Slitherlink

#' Compter le nombre de segments autour d'une case
compter_segments_autour <- function(i, j, segments_h, segments_v) {
  haut <- segments_h[i, j]
  bas <- segments_h[i + 1, j]
  gauche <- segments_v[i, j]
  droite <- segments_v[i, j + 1]
  
  somme <- 0
  if (!is.na(haut) && haut) somme <- somme + 1
  if (!is.na(bas) && bas) somme <- somme + 1
  if (!is.na(gauche) && gauche) somme <- somme + 1
  if (!is.na(droite) && droite) somme <- somme + 1
  
  return(somme)
}

#' Vérifier si la grille est correcte (règles des chiffres)
verifier_regles <- function(grille, segments_h, segments_v) {
  taille <- nrow(grille)
  
  for (i in 1:taille) {
    for (j in 1:taille) {
      chiffre <- grille[i, j]
      if (!is.na(chiffre)) {
        nb_segments <- compter_segments_autour(i, j, segments_h, segments_v)
        if (nb_segments != chiffre) {
          return(FALSE)
        }
      }
    }
  }
  return(TRUE)
}
