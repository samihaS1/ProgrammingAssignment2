# Fonction pour créer une matrice et son cache
makeCacheMatrix <- function(mat = matrix()) {
  cache <- NULL
  
  # Fonction pour fixer la valeur de la matrice
  set <- function(matrix) {
    mat <<- matrix
    cache <<- NULL  # Réinitialiser le cache lorsqu'une nouvelle matrice est définie
  }
  
  # Fonction pour obtenir la valeur de la matrice
  get <- function() mat
  
  # Fonction pour fixer l'inverse dans le cache
  setInverse <- function(inverse) {
    cache <<- inverse
  }
  
  # Fonction pour obtenir l'inverse depuis le cache
  getInverse <- function() cache
  
  # Retourner une liste de fonctions
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

# Fonction pour résoudre et mettre en cache l'inverse de la matrice
cacheSolve <- function(cacheMatrix) {
  # Récupérer l'inverse depuis le cache s'il existe
  if (!is.null(cacheMatrix$getInverse())) {
    message("Getting cached data.")
    return(cacheMatrix$getInverse())
  }
  
  # Calculer l'inverse de la matrice
  mat <- cacheMatrix$get()
  inverse <- solve(mat)
  
  # Stocker l'inverse dans le cache
  cacheMatrix$setInverse(inverse)
  
  # Renvoyer l'inverse
  inverse
}

# Exemple d'utilisation
# Créer une matrice et son cache
matCache <- makeCacheMatrix(matrix(c(1, 2, 3, 4), nrow = 2))

# Résoudre l'inverse en utilisant la fonction cacheSolve
cacheSolve(matCache)
