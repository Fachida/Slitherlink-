# R/grid.R

#' Générer une grille Slitherlink valide
#'
#' Génère aléatoirement une grille de puzzle Slitherlink en traçant
#' une région intérieure connexe par marche aléatoire fine, puis en
#' calculant les contraintes à partir de la frontière (méthode XOR).
#' La solution est garantie d'exister.
#'
#' @param taille Entier. Taille de la grille carrée (nombre de cases par côté).
#' @param niveau Caractère. Niveau de difficulté : \code{"facile"},
#'   \code{"moyen"} ou \code{"difficile"}.
#' @return Une matrice \code{taille x taille} de valeurs entières
#'   (0, 1, 2, 3) et de \code{NA}.
#' @export
generer_grille <- function(taille, niveau = "facile") {
  
  prop_visible <- switch(niveau,
                         "facile"    = 0.75,
                         "moyen"     = 0.58,
                         "difficile" = 0.42,
                         0.60
  )
  
  for (tentative in seq_len(150L)) {
    
    inside <- matrix(FALSE, nrow = taille, ncol = taille)
    i      <- sample(taille, 1L)
    j      <- sample(taille, 1L)
    inside[i, j] <- TRUE
    cible <- sample(taille:min(floor(taille * 2.5), taille^2L - 1L), 1L)
    
    for (k in seq_len(cible - 1L)) {
      dirs  <- sample(list(c(-1L,0L), c(1L,0L), c(0L,-1L), c(0L,1L)))
      moved <- FALSE
      for (d in dirs) {
        ni <- i + d[[1L]]; nj <- j + d[[2L]]
        if (ni >= 1L && ni <= taille && nj >= 1L && nj <= taille && !inside[ni, nj]) {
          inside[ni, nj] <- TRUE; i <- ni; j <- nj; moved <- TRUE; break
        }
      }
      if (!moved) {
        cells <- which(inside, arr.ind = TRUE)
        for (idx in sample(nrow(cells))) {
          ci <- cells[idx, 1L]; cj <- cells[idx, 2L]
          for (d in sample(list(c(-1L,0L),c(1L,0L),c(0L,-1L),c(0L,1L)))) {
            ni <- ci + d[[1L]]; nj <- cj + d[[2L]]
            if (ni >= 1L && ni <= taille && nj >= 1L && nj <= taille && !inside[ni, nj]) {
              inside[ni, nj] <- TRUE; i <- ni; j <- nj; moved <- TRUE; break
            }
          }
          if (moved) break
        }
        if (!moved) break
      }
    }
    
    n_pts <- taille + 1L
    sh    <- matrix(FALSE, nrow = n_pts,  ncol = taille)
    sv    <- matrix(FALSE, nrow = taille, ncol = n_pts)
    for (r in seq_len(n_pts))
      for (c in seq_len(taille)) {
        sh[r, c] <- xor(if (r > 1L) inside[r-1L,c] else FALSE,
                        if (r <= taille) inside[r,c] else FALSE)
      }
    for (r in seq_len(taille))
      for (c in seq_len(n_pts)) {
        sv[r, c] <- xor(if (c > 1L) inside[r,c-1L] else FALSE,
                        if (c <= taille) inside[r,c] else FALSE)
      }
    
    if (sum(sh) + sum(sv) < 4L) next
    
    cle <- function(r, c) paste(r, c, sep = ",")
    adj <- list()
    for (r in seq_len(n_pts)) for (c in seq_len(taille)) if (sh[r,c]) {
      k1 <- cle(r,c); k2 <- cle(r,c+1L)
      adj[[k1]] <- c(adj[[k1]],k2); adj[[k2]] <- c(adj[[k2]],k1)
    }
    for (r in seq_len(taille)) for (c in seq_len(n_pts)) if (sv[r,c]) {
      k1 <- cle(r,c); k2 <- cle(r+1L,c)
      adj[[k1]] <- c(adj[[k1]],k2); adj[[k2]] <- c(adj[[k2]],k1)
    }
    if (length(adj) == 0L) next
    
    vis <- character(0L); queue <- names(adj)[1L]
    while (length(queue) > 0L) {
      cur <- queue[1L]; queue <- queue[-1L]
      if (cur %in% vis) next
      vis <- c(vis, cur)
      for (v in adj[[cur]]) if (!(v %in% vis)) queue <- c(queue, v)
    }
    if (length(vis) != length(adj)) next
    
    g <- matrix(0L, nrow = taille, ncol = taille)
    for (ii in seq_len(taille))
      for (jj in seq_len(taille))
        g[ii, jj] <- compter_segments_autour(ii, jj, sh, sv)
    
    if (mean(g == 0L) > 0.50) next
    
    n_masque <- floor(taille^2L * (1.0 - prop_visible))
    mi <- ceiling(taille/2L); mj <- ceiling(taille/2L)
    zones <- Filter(function(z) length(z) > 0L, list(
      which(row(g) <= mi & col(g) <= mj),
      which(row(g) <= mi & col(g) >  mj),
      which(row(g) >  mi & col(g) <= mj),
      which(row(g) >  mi & col(g) >  mj)
    ))
    n_zone  <- floor(n_masque / length(zones))
    masques <- integer(0L)
    for (z in zones) { n <- min(n_zone, length(z)); if (n>0L) masques <- c(masques, sample(z,n)) }
    reste <- n_masque - length(masques)
    if (reste > 0L) {
      dispo <- setdiff(seq_len(taille^2L), masques)
      masques <- c(masques, sample(dispo, min(reste, length(dispo))))
    }
    g[masques] <- NA_integer_
    return(g)
  }
  
  message("Slitherlink : fallback rectangle")
  n_pts <- taille + 1L
  sh <- matrix(FALSE, nrow=n_pts, ncol=taille)
  sv <- matrix(FALSE, nrow=taille, ncol=n_pts)
  sh[1L,] <- TRUE; sh[n_pts,] <- TRUE
  sv[,1L] <- TRUE; sv[,n_pts] <- TRUE
  g <- matrix(0L, nrow=taille, ncol=taille)
  for (i in seq_len(taille))
    for (j in seq_len(taille))
      g[i,j] <- compter_segments_autour(i, j, sh, sv)
  g
}

#' Obtenir une grille aléatoire selon le niveau
#'
#' @param niveau Caractère. \code{"facile"} (5x5), \code{"moyen"} (6x6)
#'   ou \code{"difficile"} (7x7).
#' @return Une matrice de contraintes générée aléatoirement.
#' @export
get_grille <- function(niveau) {
  generer_grille(get_taille(niveau), niveau)
}

#' Obtenir la taille de grille associée à un niveau
#'
#' @param niveau Caractère. Niveau de difficulté.
#' @return Un entier : 5 (facile), 6 (moyen) ou 7 (difficile).
#' @export
get_taille <- function(niveau) {
  switch(niveau, "facile" = 5L, "moyen" = 6L, "difficile" = 7L, 5L)
}

#' Créer une grille vide
#'
#' @param taille Entier. Taille de la grille (défaut 5).
#' @return Une matrice de \code{NA}.
#' @export
creer_grille <- function(taille = 5L) {
  matrix(NA_integer_, nrow = taille, ncol = taille)
}

#' Initialiser les matrices de segments
#'
#' Crée deux matrices booléennes représentant les segments horizontaux
#' et verticaux, tous initialisés à \code{FALSE}.
#'
#' @param taille Entier. Taille de la grille.
#' @return Une liste avec :
#'   \describe{
#'     \item{h}{Matrice \code{(taille+1) x taille} des segments horizontaux.}
#'     \item{v}{Matrice \code{taille x (taille+1)} des segments verticaux.}
#'   }
#' @export
initialiser_segments <- function(taille) {
  list(
    h = matrix(FALSE, nrow = taille + 1L, ncol = taille),
    v = matrix(FALSE, nrow = taille,      ncol = taille + 1L)
  )
}