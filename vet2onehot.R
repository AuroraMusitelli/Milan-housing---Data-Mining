vet2onehot <- function(vet, levels) {
  L <- length(levels)
  N <- length(vet)
  mat <- matrix(0, ncol = L, nrow = N)  # inizializza con 0 invece di NA
  
  for (j in seq_along(levels)) {
    lev <- levels[j]
    mat[, j] <- as.numeric(vet == lev)
  }
  
  colnames(mat) <- levels
  return(mat)
}