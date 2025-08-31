
# ------------------------------------------------------------------------------
# Tveganja povezana z n-razsežno normalno kopulo
# ------------------------------------------------------------------------------

generiraj_normalno_kopulo <- function(d, n, Sigma, inv_cdfs) {
  
  # 1. Choleski razcep korelacijske matrike
  L <- chol(Sigma) # ukaz chol vrne zgornje trikotno matriko
  L <- t(L)
  
  # 2. Generiraj standardno normalne vrednosti: Z ~ N(0, I)
  Z <- matrix(rnorm(n * d), nrow = n, ncol = d)
  
  # 3. Pretvori v Y = L * Z (n x m matrika)
  Y <- L %*% Z
  
  # 4. Transformacija v uniformne vrednosti preko Φ
  U <- pnorm(Y)
  
  # 5. Inverzne robne transformacije
  M <- matrix(NA, nrow = n, ncol = d)
  for (i in 1:n) {
    M[i, ] <- inv_cdfs[[i]](U[i, ])
  }
  
  return(list(Z = Z, Y = Y, U = U, M = M))
}

# ------------------------------------------------------------------------------
# Hierarhična agregacija tveganj
# ------------------------------------------------------------------------------

generiraj_hierarhicno_odvisnost <- function(drevo, robne_porazdelitve, utezi, parametri, tip_kopule, d) {
  
  vzorec <- list()
  
  # 1. Generiraj n vzorcev robnih porazdelitev
  for (list in drevo$listi) {
    print(paste0("Generiram vzorec porazdelitve lista ", list))
    vzorec[[list]] <- robne_porazdelitve[[list]](d)
  }
  
  for (vozlisce in drevo$notranja_vozlisca){
    
    print(paste0("Generiram vzorec porazdelitve notranjega vozlisca ", vozlisce))
    otroci <- drevo$otroci[[vozlisce]]
    vzorec_otrok <- lapply(otroci, function(o) vzorec[[o]])
    utezi_otrok <- utezi[[vozlisce]]
    
    print(paste0("Ocenjujem najboljši parameter "))
    
    # 2. Oceni najboljši parameter theta
    if (tip_kopule == "Gumbel"){
      theta <- parametri[[vozlisce]]
      copula <- gumbelCopula(param = theta, dim = 2)
      print(paste0("Simuliram kopulo s podanim parametrom theta = ", theta))
    } else if (tip_kopule == 'Clayton_survival') {
      theta <- parametri[[vozlisce]]
      copula <- rotCopula(claytonCopula(param = theta, dim = 2))
      print(paste0("Simuliram kopulo s podanim parametrom theta = ", theta))
    } else {
      stop("Nepodprt tip kopule.")
    }
    
    # 3. Generiraj d vzorcev kopule C_vozlisce(best_theta)
    U <- rCopula(d, copula)
    
    # 4. Izracunaj range 
    ranks <- apply(U, 2, rank)
    
    # 5. Apliciraj permutacije na vzorec otork
    sortiran_otrok_1 <- (sort(vzorec_otrok[[1]]))[ranks[, 1]]
    sortiran_otrok_2 <- (sort(vzorec_otrok[[2]]))[ranks[, 2]]
    
    # 6. Sestavi vsoto s primernimi utezmi                                              
    vsota <- utezi_otrok[[1]] * sortiran_otrok_1 + utezi_otrok[[2]] * sortiran_otrok_2
    
    vzorec[[vozlisce]] <- vsota
  }
  
  return(vzorec[[drevo$koren]])
}

