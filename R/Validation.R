# R/validation.R

#' Compter les segments actifs autour d'une case
#'
#' @param i Entier. Ligne de la case.
#' @param j Entier. Colonne de la case.
#' @param segments_h Matrice booléenne des segments horizontaux.
#' @param segments_v Matrice booléenne des segments verticaux.
#' @return Un entier entre 0 et 4.
#' @export
compter_segments_autour <- function(i, j, segments_h, segments_v) {
  somme <- 0L
  if (segments_h[i,     j]) somme <- somme + 1L
  if (segments_h[i + 1, j]) somme <- somme + 1L
  if (segments_v[i, j    ]) somme <- somme + 1L
  if (segments_v[i, j + 1]) somme <- somme + 1L
  somme
}

#' Vérifier que les contraintes numériques sont respectées
#'
#' Vérifie que chaque case numérotée possède exactement le bon
#' nombre de segments actifs sur ses 4 côtés.
#'
#' @param grille Matrice de contraintes (0,1,2,3,NA).
#' @param segments_h Matrice booléenne des segments horizontaux.
#' @param segments_v Matrice booléenne des segments verticaux.
#' @return \code{TRUE} si toutes les contraintes sont respectées,
#'   \code{FALSE} sinon.
#' @export
verifier_regles <- function(grille, segments_h, segments_v) {
  taille <- nrow(grille)
  for (i in 1:taille)
    for (j in 1:taille) {
      val <- grille[i, j]
      if (!is.na(val))
        if (compter_segments_autour(i, j, segments_h, segments_v) != val)
          return(FALSE)
    }
  TRUE
}

#' Détecter les cases en erreur
#'
#' @param grille Matrice de contraintes (0,1,2,3,NA).
#' @param segments_h Matrice booléenne des segments horizontaux.
#' @param segments_v Matrice booléenne des segments verticaux.
#' @return Une matrice booléenne : \code{TRUE} pour les cases en erreur.
#' @export
detecter_erreurs <- function(grille, segments_h, segments_v) {
  taille  <- nrow(grille)
  erreurs <- matrix(FALSE, nrow = taille, ncol = taille)
  for (i in 1:taille)
    for (j in 1:taille) {
      val <- grille[i, j]
      if (!is.na(val))
        if (compter_segments_autour(i, j, segments_h, segments_v) != val)
          erreurs[i, j] <- TRUE
    }
  erreurs
}

#' Calculer le degré d'un point de la grille
#'
#' Le degré d'un point est le nombre de segments actifs qui lui sont
#' adjacents. Une boucle valide impose un degré de 0 ou 2 en tout point.
#'
#' @param r Entier. Ligne du point (1 à taille+1).
#' @param c Entier. Colonne du point (1 à taille+1).
#' @param segments_h Matrice booléenne des segments horizontaux.
#' @param segments_v Matrice booléenne des segments verticaux.
#' @return Un entier entre 0 et 4.
#' @export
degre_point <- function(r, c, segments_h, segments_v) {
  taille <- ncol(segments_h)
  deg    <- 0L
  if (c <= taille && segments_h[r, c])     deg <- deg + 1L
  if (c > 1       && segments_h[r, c - 1]) deg <- deg + 1L
  if (r <= taille && segments_v[r, c])     deg <- deg + 1L
  if (r > 1       && segments_v[r - 1, c]) deg <- deg + 1L
  deg
}

#' Vérifier que tous les points ont un degré valide
#'
#' Chaque point de la grille doit avoir exactement 0 ou 2 segments
#' actifs adjacents (pas d'impasse ni de branchement).
#'
#' @param segments_h Matrice booléenne des segments horizontaux.
#' @param segments_v Matrice booléenne des segments verticaux.
#' @return \code{TRUE} si tous les degrés sont valides, \code{FALSE} sinon.
#' @export
verifier_degres <- function(segments_h, segments_v) {
  taille   <- ncol(segments_h)
  n_points <- taille + 1L
  for (r in 1:n_points)
    for (c in 1:n_points) {
      d <- degre_point(r, c, segments_h, segments_v)
      if (d != 0L && d != 2L) return(FALSE)
    }
  TRUE
}

#' Vérifier que les segments forment une seule boucle fermée
#'
#' Effectue un parcours en largeur (BFS) depuis le premier segment
#' actif et vérifie que tous les segments actifs sont connectés.
#'
#' @param segments_h Matrice booléenne des segments horizontaux.
#' @param segments_v Matrice booléenne des segments verticaux.
#' @return \code{TRUE} si une seule boucle fermée, \code{FALSE} sinon.
#' @export
verifier_boucle_unique <- function(segments_h, segments_v) {
  taille   <- ncol(segments_h)
  n_points <- taille + 1L
  cle      <- function(r, c) paste(r, c, sep = ",")
  adj      <- list()
  
  for (r in 1:n_points) for (c in 1:taille) if (segments_h[r,c]) {
    k1 <- cle(r,c); k2 <- cle(r,c+1)
    adj[[k1]] <- c(adj[[k1]],k2); adj[[k2]] <- c(adj[[k2]],k1)
  }
  for (r in 1:taille) for (c in 1:n_points) if (segments_v[r,c]) {
    k1 <- cle(r,c); k2 <- cle(r+1,c)
    adj[[k1]] <- c(adj[[k1]],k2); adj[[k2]] <- c(adj[[k2]],k1)
  }
  
  if (length(adj) == 0L) return(FALSE)
  
  visites <- character(0); a_visiter <- names(adj)[1]
  while (length(a_visiter) > 0) {
    courant   <- a_visiter[1]; a_visiter <- a_visiter[-1]
    if (courant %in% visites) next
    visites   <- c(visites, courant)
    for (v in adj[[courant]])
      if (!(v %in% visites)) a_visiter <- c(a_visiter, v)
  }
  length(visites) == length(adj)
}

#' Valider une solution complète de Slitherlink
#'
#' Vérifie les trois conditions nécessaires et suffisantes :
#' \enumerate{
#'   \item Les contraintes numériques sont respectées.
#'   \item Chaque point a un degré de 0 ou 2 (pas d'impasse ni de fourche).
#'   \item Les segments forment une seule boucle fermée.
#' }
#'
#' @param grille Matrice de contraintes (0,1,2,3,NA).
#' @param segments_h Matrice booléenne des segments horizontaux.
#' @param segments_v Matrice booléenne des segments verticaux.
#' @return \code{TRUE} si la solution est valide, \code{FALSE} sinon.
#' @export
verifier_solution_complete <- function(grille, segments_h, segments_v) {
  if (!verifier_regles(grille, segments_h, segments_v))  return(FALSE)
  if (!verifier_degres(segments_h, segments_v))          return(FALSE)
  if (!verifier_boucle_unique(segments_h, segments_v))   return(FALSE)
  TRUE
}