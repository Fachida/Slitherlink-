# R/solver.R
# Solveur ILP pour Slitherlink — utilise le package lpSolve
#
# FORMULATION :
#   Variables x_s ∈ {0,1} : chaque segment est actif ou non
#   Variables y_p ∈ {0,1} : chaque point appartient à la boucle ou non
#
#   Contraintes cases   : Σ x_s autour de (i,j) = grille[i,j]
#   Contraintes degrés  : Σ x_s autour du point p = 2 * y_p
#                         → garantit que chaque point a 0 ou 2 segments
#
#   Connexité : l'ILP peut trouver plusieurs mini-boucles valides localement.
#   On détecte ça et on ajoute des contraintes d'élimination de sous-boucles
#   (subtour elimination, comme en TSP) jusqu'à obtenir une seule boucle.

# -------------------------------------------------------
# 1. Trouver les composantes connexes d'une solution
# -------------------------------------------------------


trouver_composantes <- function(segments_h, segments_v) {
  taille   <- ncol(segments_h)
  n_points <- taille + 1
  cle      <- function(r, c) paste(r, c, sep = ",")
  
  # Liste d'adjacence des points actifs
  adj <- list()
  for (r in 1:n_points)
    for (c in 1:taille)
      if (segments_h[r, c]) {
        k1 <- cle(r, c); k2 <- cle(r, c + 1)
        adj[[k1]] <- c(adj[[k1]], k2)
        adj[[k2]] <- c(adj[[k2]], k1)
      }
  for (r in 1:taille)
    for (c in 1:n_points)
      if (segments_v[r, c]) {
        k1 <- cle(r, c); k2 <- cle(r + 1, c)
        adj[[k1]] <- c(adj[[k1]], k2)
        adj[[k2]] <- c(adj[[k2]], k1)
      }
  
  if (length(adj) == 0) return(list())
  
  # BFS pour trouver toutes les composantes
  non_visites <- names(adj)
  composantes <- list()
  
  while (length(non_visites) > 0) {
    depart     <- non_visites[1]
    file       <- depart
    composante <- character(0)
    
    while (length(file) > 0) {
      courant <- file[1]
      file    <- file[-1]
      if (courant %in% composante) next
      composante <- c(composante, courant)
      for (v in adj[[courant]])
        if (!(v %in% composante)) file <- c(file, v)
    }
    
    composantes <- c(composantes, list(composante))
    non_visites <- setdiff(non_visites, composante)
  }
  
  return(composantes)
}

# -------------------------------------------------------
# 2. Solveur principal ILP avec élimination de sous-boucles
# -------------------------------------------------------
resoudre <- function(grille, timeout_sec = 60) {
  
  if (!requireNamespace("lpSolve", quietly = TRUE)) {
    stop("Package manquant. Exécute : install.packages('lpSolve')")
  }
  
  taille   <- nrow(grille)
  n_points <- taille + 1
  
  # --- Indices des variables ---
  # x_h[r,c] : segment horizontal, r ∈ 1..n_points, c ∈ 1..taille
  # x_v[r,c] : segment vertical,   r ∈ 1..taille,   c ∈ 1..n_points
  # y[r,c]   : variable point,     r ∈ 1..n_points, c ∈ 1..n_points
  n_h    <- n_points * taille
  n_v    <- taille   * n_points
  n_segs <- n_h + n_v
  n_y    <- n_points^2
  n_vars <- n_segs + n_y
  
  idx_h <- function(r, c) (r - 1) * taille   + c
  idx_v <- function(r, c) n_h + (r - 1) * n_points + c
  idx_y <- function(r, c) n_segs + (r - 1) * n_points + c
  
  # Indices des 4 segments autour de la case (i,j)
  segs_case <- function(i, j)
    c(idx_h(i, j), idx_h(i + 1, j), idx_v(i, j), idx_v(i, j + 1))
  
  # Indices des segments adjacents au point (r,c)
  segs_point <- function(r, c) {
    idx <- c()
    if (c <= taille) idx <- c(idx, idx_h(r,     c))
    if (c > 1)       idx <- c(idx, idx_h(r,     c - 1))
    if (r <= taille) idx <- c(idx, idx_v(r,     c))
    if (r > 1)       idx <- c(idx, idx_v(r - 1, c))
    return(idx)
  }
  
  # --- Contraintes de base (cases + degrés) ---
  base_rows <- list()
  base_dirs <- character(0)
  base_rhs  <- numeric(0)
  
  # Contraintes cases
  for (i in 1:taille)
    for (j in 1:taille) {
      val <- grille[i, j]
      if (!is.na(val)) {
        row <- rep(0, n_vars)
        row[segs_case(i, j)] <- 1
        base_rows <- c(base_rows, list(row))
        base_dirs <- c(base_dirs, "=")
        base_rhs  <- c(base_rhs, val)
      }
    }
  
  # Contraintes degrés : Σ x_s - 2 * y_p = 0
  for (r in 1:n_points)
    for (c in 1:n_points) {
      row <- rep(0, n_vars)
      row[segs_point(r, c)] <-  1
      row[idx_y(r, c)]      <- -2
      base_rows <- c(base_rows, list(row))
      base_dirs <- c(base_dirs, "=")
      base_rhs  <- c(base_rhs, 0)
    }
  
  # Objectif : minimiser le nombre de segments actifs
  obj <- c(rep(1, n_segs), rep(0, n_y))
  
  # Contraintes d'élimination de sous-boucles (ajoutées itérativement)
  extra_rows <- list()
  extra_dirs <- character(0)
  extra_rhs  <- numeric(0)
  
  debut <- proc.time()["elapsed"]
  
  repeat {
    
    # Timeout
    if ((proc.time()["elapsed"] - debut) > timeout_sec)
      return(list(resolu = FALSE, message = "timeout"))
    
    # Construire la matrice de contraintes complète
    toutes_rows <- c(base_rows, extra_rows)
    toutes_dirs <- c(base_dirs, extra_dirs)
    toutes_rhs  <- c(base_rhs,  extra_rhs)
    
    n_contr <- length(toutes_rows)
    mat     <- matrix(0, nrow = n_contr, ncol = n_vars)
    for (k in 1:n_contr) mat[k, ] <- toutes_rows[[k]]
    
    # Résoudre l'ILP
    sol <- lpSolve::lp("min", obj, mat, toutes_dirs, toutes_rhs, all.bin = TRUE)
    
    if (sol$status != 0)
      return(list(resolu = FALSE, message = "aucune solution"))
    
    # Extraire la solution
    x          <- sol$solution
    segments_h <- matrix(FALSE, nrow = n_points, ncol = taille)
    segments_v <- matrix(FALSE, nrow = taille,   ncol = n_points)
    
    for (r in 1:n_points)
      for (c in 1:taille)
        segments_h[r, c] <- x[idx_h(r, c)] > 0.5
    
    for (r in 1:taille)
      for (c in 1:n_points)
        segments_v[r, c] <- x[idx_v(r, c)] > 0.5
    
    # Vérifier la connexité
    composantes <- trouver_composantes(segments_h, segments_v)
    
    # Une seule composante → solution valide !
    if (length(composantes) <= 1)
      return(list(resolu     = TRUE,
                  segments_h = segments_h,
                  segments_v = segments_v))
    
    # Plusieurs boucles → éliminer la plus petite (subtour elimination)
    # Contrainte ajoutée : Σ x_s dans la sous-boucle ≤ |sous-boucle| - 1
    tailles   <- sapply(composantes, length)
    petite    <- composantes[[which.min(tailles)]]
    cle_to_rc <- function(k) as.integer(strsplit(k, ",")[[1]])
    
    segs_sb <- c()
    for (pt_str in petite) {
      pt <- cle_to_rc(pt_str)
      r  <- pt[1]; c <- pt[2]
      if (c <= taille && segments_h[r, c])     segs_sb <- c(segs_sb, idx_h(r, c))
      if (c > 1       && segments_h[r, c - 1]) segs_sb <- c(segs_sb, idx_h(r, c - 1))
      if (r <= taille && segments_v[r, c])     segs_sb <- c(segs_sb, idx_v(r, c))
      if (r > 1       && segments_v[r - 1, c]) segs_sb <- c(segs_sb, idx_v(r - 1, c))
    }
    segs_sb <- unique(segs_sb)
    
    row <- rep(0, n_vars)
    row[segs_sb] <- 1
    extra_rows <- c(extra_rows, list(row))
    extra_dirs <- c(extra_dirs, "<=")
    extra_rhs  <- c(extra_rhs,  length(segs_sb) - 1)
  }
}