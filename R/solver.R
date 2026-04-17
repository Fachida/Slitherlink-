# R/solver.R

#' Trouver les composantes connexes de la boucle
#'
#' Utilisé en interne par \code{resoudre()} pour détecter les
#' sous-boucles et ajouter des contraintes d'élimination.
#'
#' @param segments_h Matrice booléenne des segments horizontaux.
#' @param segments_v Matrice booléenne des segments verticaux.
#' @return Une liste de vecteurs de caractères, chacun représentant
#'   une composante connexe (ensemble de clés de points).
#' @keywords internal
trouver_composantes <- function(segments_h, segments_v) {
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
  
  if (length(adj) == 0L) return(list())
  
  non_vis <- names(adj); comps <- list()
  while (length(non_vis) > 0) {
    file <- non_vis[1]; comp <- character(0)
    while (length(file) > 0) {
      cur <- file[1]; file <- file[-1]
      if (cur %in% comp) next
      comp <- c(comp, cur)
      for (v in adj[[cur]]) if (!(v %in% comp)) file <- c(file, v)
    }
    comps   <- c(comps, list(comp))
    non_vis <- setdiff(non_vis, comp)
  }
  comps
}

#' Résoudre une grille Slitherlink par programmation linéaire en nombres entiers (ILP)
#'
#' Formule le problème Slitherlink comme un ILP et le résout avec
#' \code{lpSolve}. La connexité est assurée par élimination itérative
#' des sous-boucles (méthode des coupes de Benders).
#'
#' Les variables sont :
#' \itemize{
#'   \item \eqn{x_s \in \{0,1\}} : chaque segment est actif ou non.
#'   \item \eqn{y_p \in \{0,1\}} : chaque point appartient à la boucle ou non.
#' }
#'
#' Les contraintes sont :
#' \itemize{
#'   \item Contraintes cases : \eqn{\sum x_s = \text{chiffre}} autour de chaque case numérotée.
#'   \item Contraintes degrés : \eqn{\sum x_s = 2 y_p} en chaque point.
#'   \item Élimination de sous-boucles : ajoutées itérativement jusqu'à
#'     obtenir une seule boucle connexe.
#' }
#'
#' @param grille Matrice de contraintes (0,1,2,3,NA).
#' @param timeout_sec Nombre maximum de secondes autorisées (défaut 60).
#' @return Une liste avec :
#'   \describe{
#'     \item{resolu}{Booléen. \code{TRUE} si une solution a été trouvée.}
#'     \item{segments_h}{Matrice booléenne des segments horizontaux de la solution.}
#'     \item{segments_v}{Matrice booléenne des segments verticaux de la solution.}
#'     \item{message}{Caractère. \code{"timeout"} ou \code{"aucune solution"} en cas d'échec.}
#'   }
#' @export
#' @examples
#' \dontrun{
#' source("R/validation.R")
#' source("R/grid.R")
#' source("R/solver.R")
#' grille  <- get_grille("facile")
#' resultat <- resoudre(grille)
#' if (resultat$resolu) print("Résolu !")
#' }
resoudre <- function(grille, timeout_sec = 60) {
  
  if (!requireNamespace("lpSolve", quietly = TRUE))
    stop("Installe lpSolve : install.packages('lpSolve')")
  
  taille   <- nrow(grille)
  n_points <- taille + 1L
  n_h      <- n_points * taille
  n_v      <- taille   * n_points
  n_segs   <- n_h + n_v
  n_y      <- n_points^2L
  n_vars   <- n_segs + n_y
  
  idx_h <- function(r, c) (r - 1L) * taille    + c
  idx_v <- function(r, c) n_h + (r - 1L) * n_points + c
  idx_y <- function(r, c) n_segs + (r - 1L) * n_points + c
  
  segs_case <- function(i, j)
    c(idx_h(i,j), idx_h(i+1,j), idx_v(i,j), idx_v(i,j+1))
  
  segs_point <- function(r, c) {
    idx <- integer(0)
    if (c <= taille) idx <- c(idx, idx_h(r,   c))
    if (c > 1)       idx <- c(idx, idx_h(r,   c-1))
    if (r <= taille) idx <- c(idx, idx_v(r,   c))
    if (r > 1)       idx <- c(idx, idx_v(r-1, c))
    idx
  }
  
  base_rows <- list(); base_dirs <- character(0); base_rhs <- numeric(0)
  
  for (i in 1:taille) for (j in 1:taille) {
    val <- grille[i,j]
    if (!is.na(val)) {
      row <- rep(0, n_vars); row[segs_case(i,j)] <- 1
      base_rows <- c(base_rows, list(row))
      base_dirs <- c(base_dirs, "="); base_rhs <- c(base_rhs, val)
    }
  }
  
  for (r in 1:n_points) for (c in 1:n_points) {
    row <- rep(0, n_vars)
    row[segs_point(r,c)] <-  1
    row[idx_y(r,c)]      <- -2
    base_rows <- c(base_rows, list(row))
    base_dirs <- c(base_dirs, "="); base_rhs <- c(base_rhs, 0)
  }
  
  obj        <- c(rep(1, n_segs), rep(0, n_y))
  extra_rows <- list(); extra_dirs <- character(0); extra_rhs <- numeric(0)
  debut      <- proc.time()["elapsed"]
  
  repeat {
    if ((proc.time()["elapsed"] - debut) > timeout_sec)
      return(list(resolu = FALSE, message = "timeout"))
    
    toutes_rows <- c(base_rows, extra_rows)
    toutes_dirs <- c(base_dirs, extra_dirs)
    toutes_rhs  <- c(base_rhs,  extra_rhs)
    n_c <- length(toutes_rows)
    mat <- matrix(0, nrow = n_c, ncol = n_vars)
    for (k in 1:n_c) mat[k,] <- toutes_rows[[k]]
    
    sol <- lpSolve::lp("min", obj, mat, toutes_dirs, toutes_rhs, all.bin = TRUE)
    if (sol$status != 0) return(list(resolu = FALSE, message = "aucune solution"))
    
    x  <- sol$solution
    sh <- matrix(FALSE, nrow = n_points, ncol = taille)
    sv <- matrix(FALSE, nrow = taille,   ncol = n_points)
    for (r in 1:n_points) for (c in 1:taille)   sh[r,c] <- x[idx_h(r,c)] > 0.5
    for (r in 1:taille)   for (c in 1:n_points)  sv[r,c] <- x[idx_v(r,c)] > 0.5
    
    comps <- trouver_composantes(sh, sv)
    if (length(comps) <= 1L)
      return(list(resolu = TRUE, segments_h = sh, segments_v = sv))
    
    # Éliminer la plus petite sous-boucle
    tailles <- sapply(comps, length)
    petite  <- comps[[which.min(tailles)]]
    dec_cle <- function(k) as.integer(strsplit(k, ",")[[1]])
    
    segs_sb <- integer(0)
    for (pt_str in petite) {
      pt <- dec_cle(pt_str); r <- pt[1]; c <- pt[2]
      if (c <= taille && sh[r,c])     segs_sb <- c(segs_sb, idx_h(r,c))
      if (c > 1       && sh[r,c-1])   segs_sb <- c(segs_sb, idx_h(r,c-1))
      if (r <= taille && sv[r,c])     segs_sb <- c(segs_sb, idx_v(r,c))
      if (r > 1       && sv[r-1,c])   segs_sb <- c(segs_sb, idx_v(r-1,c))
    }
    segs_sb <- unique(segs_sb)
    row <- rep(0, n_vars); row[segs_sb] <- 1
    extra_rows <- c(extra_rows, list(row))
    extra_dirs <- c(extra_dirs, "<=")
    extra_rhs  <- c(extra_rhs,  length(segs_sb) - 1)
  }
}