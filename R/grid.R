# R/grid.R

creer_grille <- function(taille = 5) {
  matrix(NA, nrow = taille, ncol = taille)
}

get_grille <- function(niveau) {
  if (niveau == "facile") {
    matrix(c(NA,2,NA,3,NA,1,NA,2,NA,1,NA,3,NA,2,NA,2,NA,1,NA,3,NA,0,NA,2,NA), nrow=5, byrow=TRUE)
  } else if (niveau == "moyen") {
    matrix(c(NA,2,NA,NA,3,NA,NA,1,NA,NA,2,NA,NA,2,NA,NA,3,NA,NA,1,NA,2,NA,NA,NA,NA,NA,3,NA,1,NA,NA,2,NA,NA,3,NA,NA,1,NA,NA,0,NA,NA,2,NA,NA,3,NA), nrow=7, byrow=TRUE)
  } else {
    matrix(c(NA,NA,2,NA,NA,3,NA,NA,1,NA,1,NA,NA,2,NA,NA,0,NA,2,NA,NA,3,NA,NA,1,NA,NA,NA,NA,1,NA,NA,2,NA,NA,3,3,NA,NA,1,NA,NA,2,NA,NA,NA,2,NA,NA,0,NA,NA,1,NA,1,NA,NA,2,NA,NA,3,NA,NA,NA,0,NA,NA,1,NA,NA,2,NA,NA,NA,3,NA,NA,2,NA,NA,1), nrow=9, byrow=TRUE)
  }
}

get_taille <- function(niveau) {
  if (niveau == "facile") return(5)
  if (niveau == "moyen") return(7)
  return(9)
}
