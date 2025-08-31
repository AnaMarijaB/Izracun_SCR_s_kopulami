library(xtable) # izpis tabel v latex kodi
library(ggplot2)
library(dplyr)
library(tidyr)
library(skimr)
library(psych)
library(PerformanceAnalytics) # risanje chart.Corr
library(VineCopula) 
library(TSP)

set.seed(123)

############################      PODATKI      #################################

podatki <- read.csv("C:/Users/ambelingar/Magistrska_R/podatki/podatki_2.csv", sep = ';', dec = ',', header = TRUE)

podatki_CR <- podatki %>%
  filter(Postavka == 'CR') %>%
  dplyr::select(Postavka, Leto, Zavarovalnica, kratica, Znesek) %>%
  mutate(Znesek = if_else(Znesek < 0, 0, Znesek)) %>%
  pivot_wider(names_from = kratica, values_from = Znesek) 

podatki_PREM <- podatki %>%
  filter(Postavka == 'PREMIJA') %>%
  dplyr::select(Postavka, Leto, Zavarovalnica, kratica, Znesek) %>%
  mutate(Znesek = if_else(Znesek < 0, 0, Znesek)) %>%
  pivot_wider(names_from = kratica, values_from = Znesek) 

podatki_CR_num <- as.matrix(podatki_CR %>% dplyr::select(AO, DMZ, POZ, SZO, ZP)) # škodni količniki
podatki_PREM_num <- as.matrix(podatki_PREM %>% dplyr::select(AO, DMZ, POZ, SZO, ZP)) # premije
podatki_OB_num <- podatki_CR_num * podatki_PREM_num # obveznosti

# to potrebujemo samo za izris grafa
podatki_OB <- podatki_PREM
podatki_OB[ , 4 : 8] <- podatki_OB_num

# osnovna deskriptivna statistika (Tabela 6)
skim(podatki_CR_num * 100)
print(describe(podatki_CR_num * 100))

# osnovna struktura odvisnosti
chart.Correlation(podatki_CR_num, histogram = TRUE, pch = 19)

#-----------------------------      UTEZI     ----------------------------------
# Tabela 5

segm_PREM <- colSums(podatki_PREM_num)
segm_OB <- colSums(podatki_OB_num)
segm_OB <- round(segm_OB/segm_PREM, 5)
segm_UT <- round(segm_PREM / sum(segm_PREM) * 100, 5)

#------------------ KOMBINIRAN KOLICNIK VSOTE - L_prem -------------------------

podatki_UT_CR <- podatki_OB %>%
  left_join(
    podatki_PREM %>%
      mutate(PREM_sum = (AO + DMZ + POZ + SZO + ZP)) %>%
      select(Leto, Zavarovalnica, PREM_sum),
    by = c('Leto', 'Zavarovalnica')
  ) %>%
  mutate(across(c(AO, DMZ, POZ, SZO, ZP), ~ .x * 100 / PREM_sum)) %>%
  select(Leto, Zavarovalnica, AO, DMZ, POZ, SZO, ZP)

podatki_UT_CR_num <- as.matrix(podatki_UT_CR %>% dplyr::select(AO, DMZ, POZ, SZO, ZP))
L_prem_emp <- apply(podatki_UT_CR_num, 1 , sum)
SCR_emp <- quantile(L_prem_emp, probs = 0.995) - mean(L_prem_emp) # 40.10481
SCR_emp

# deskriptivna statistika
skim(L_prem_emp)
print(describe(L_prem_emp))

######################    ROBNE PORAZDELITVE      ##############################

# Funkcija za iskanje najboljših parametrov lognormalne porazdelitve
najdi_najboljso_porazdelitev <- function(podatki) {
  podatki <- podatki[podatki > 0]
  podatki <- podatki[!is.na(podatki)]
  log_podatki <- log(podatki)
  mu <- mean(log_podatki)
  sigma <- mean((log_podatki - mu)^2)
  
  return(c(meanlog = mu, sdlog = sigma))
}

porazdelitve <- apply(as.matrix(podatki_CR_num), 2, function(col) najdi_najboljso_porazdelitev(col))
print(porazdelitve)

mu_log_vec <- round(as.numeric(porazdelitve["meanlog", ]), 5)
var_log_vec <- round(as.numeric(porazdelitve["sdlog", ]), 5)
sd_log_vec <- sqrt(var_log_vec)

# porazdelitvene funkcije
funk_porazdelitve <- lapply(1:5, function(i) {
  mu <- mu_log_vec[i]
  sigma <- sd_log_vec[i]
  return(function(n) rlnorm(n, meanlog = mu, sdlog = sigma))
})
names(funk_porazdelitve) <- colnames(porazdelitve)

# inverzne porazdelitvene funkcije
inv_porazdelitve <- lapply(1:5, function(i) {
  function(u) qlnorm(u, meanlog = mu_log_vec[i], sdlog = sd_log_vec[i])
})

######################    MODELIRANJE ODVISNOSTI      ##########################

d <- 5000
n <- 5

# ------------------------------------------------------------------------------
# ----------------------- Neodvisna tveganja -----------------------------------
# ------------------------------------------------------------------------------

kop_neodv_U <- matrix(runif(n * d), nrow = n, ncol = d)
kop_neodv_M <- t(sapply(1:n, function(i) inv_porazdelitve[[i]](kop_neodv_U[i, ])))
rownames(kop_neodv_M) <- c('AO', 'DMZ', 'POZ', 'SZO', 'ZP')

# da gre za neodvisno kopulo preverimo s korelacijsko matriko
cor(t(kop_neodv_M), method = 'pearson')
cor(t(kop_neodv_M), method = 'spearman')

# ko vzorce utežimo, se korelacije ne spremenijo
A_neodv <- t(kop_neodv_M) %*% diag(segm_UT)
cor(A_neodv, method = 'pearson')

L_prem_neodv <- apply(A_neodv, 1 , sum)
SCR_neodv <- quantile(L_prem_neodv, probs = 0.995) - mean(L_prem_neodv) # 40.56215
SCR_neodv

# ------------------------------------------------------------------------------
# ----------------- 5-razsežna normalna kopula ---------------------------------
# ------------------------------------------------------------------------------

matrika_korelacij <- matrix(c(
  1.00, 0.50, 0.25, 0.50, 0.25, 
  0.50, 1.00, 0.25, 0.25, 0.50, 
  0.25, 0.25, 1.00, 0.25, 0.50, 
  0.50, 0.25, 0.25, 1.00, 0.25, 
  0.25, 0.50, 0.50, 0.25, 1.00), nrow = 5, ncol = 5, byrow = TRUE)

rownames(matrika_korelacij) <- colnames(matrika_korelacij) <- c('AO', 'DMZ', 'POZ', 'SZO', 'ZP')

kop_norm <-  generiraj_normalno_kopulo(d, n, matrika_korelacij, inv_porazdelitve)
kop_norm_M <- kop_norm$M

# kvaliteta ujemanja korelacijske matrike vzorca s predpisano matriko
cor(t(kop_norm_M), method = 'pearson')
cor(t(kop_norm_M), method = 'spearman')

# ko vzorce utežimo, se korelacije ne spremenijo
A_norm <- t(kop_norm_M) %*% diag(segm_UT)
cor(A_norm, method = 'pearson')
cor(A_norm, method = 'spearman')

L_prem_norm <- apply(A_norm, 1 , sum)
SCR_norm <- quantile(L_prem_norm, probs = 0.995) - mean(L_prem_norm) # 61.39575 
SCR_norm

# ------------------------------------------------------------------------------
# ----------------- Hierarhična agregacija tveganj -----------------------------
# ------------------------------------------------------------------------------

# Funkcija za izračun relativnih uteži v drevesu
izracunaj_relativne_utezi <- function(drevo, utezi_listov) {
  
  vse_utezi <- as.list(utezi_listov)
  relativne_utezi_vozlisca <- list()
  
  # Izračun skupne uteži za vsako notranje vozlišče
  for (vozlisce in drevo$notranja_vozlisca) {
    otroci <- drevo$otroci[[vozlisce]]
    vsota_utezi_otrok <- sum(unlist(vse_utezi[otroci]))
    vse_utezi[[vozlisce]] <- vsota_utezi_otrok
  }
  
  # Izračun relativnih uteži glede na posamezno notranje vozlišče
  for (vozlisce in drevo$notranja_vozlisca) {
    otroci <- drevo$otroci[[vozlisce]]
    vsota_utezi_otrok <- unlist(vse_utezi[vozlisce])
    relativne_utezi <- unlist(vse_utezi[otroci]) / vsota_utezi_otrok
    relativne_utezi_vozlisca[[vozlisce]] <- relativne_utezi
  }
  
  return(relativne_utezi_vozlisca)
}

drevo <- list(
  koren = "T",
  listi = c('AO', 'DMZ', 'POZ', 'SZO', 'ZP'),
  notranja_vozlisca = c('S1', 'S2', 'S3', 'T'),
  otroci = list(
    T = c('S3', 'POZ'),
    S3 = c('S2', 'SZO'),
    S2 = c('S1', 'ZP'),
    S1 =c('AO', 'DMZ')
  )
)

podatki_CR_num_hierar <- as.data.frame(podatki_CR_num) %>%
  mutate(S1 = AO + DMZ,
         S2 = S1 + ZP,
         S3 = S2 + SZO,
         T = S3 + POZ)

U_podatki_CR_hierar <- pobs(podatki_CR_num_hierar)

relativne_utezi <- izracunaj_relativne_utezi(drevo, segm_UT)

#---------------------- Gumbel kopula ------------------------------------------

# parametri določeni z MLE U_podatki_CR
param_gumbel_S1_mle <- fitCopula(gumbelCopula(), U_podatki_CR_hierar[, c("AO", "DMZ")], method = "ml") # 1.908
param_gumbel_S2_mle <- fitCopula(gumbelCopula(), U_podatki_CR_hierar[, c("S1", "ZP")], method = "ml") # 1.282 
param_gumbel_S3_mle <- fitCopula(gumbelCopula(), U_podatki_CR_hierar[, c("S2", "SZO")], method = "ml") # 1.028
param_gumbel_T1_mle <- fitCopula(gumbelCopula(), U_podatki_CR_hierar[, c("S3", "POZ")], method = "ml") # 1.063

# parametri določeni z itau metodo
param_gumbel_S1_itau <- fitCopula(gumbelCopula(), U_podatki_CR_hierar[, c("AO", "DMZ")], method = "itau") # 1.643
param_gumbel_S2_itau <- fitCopula(gumbelCopula(), U_podatki_CR_hierar[, c("S1", "ZP")], method = "itau") # 1.253
param_gumbel_S3_itau <- fitCopula(gumbelCopula(), U_podatki_CR_hierar[, c("S2", "SZO")], method = "itau") # 1.01
param_gumbel_T_itau <- fitCopula(gumbelCopula(), U_podatki_CR_hierar[, c("S3", "POZ")], method = "itau") # 1.01

parametri_gumbel_mle <- list(
  S1 = 1.908,
  S2 = 1.282,
  S3= 1.028,
  T = 1.063
)

parametri_gumbel_itau <- list(
  S1 = 1.643,
  S2 = 1.253,
  S3= 1.01,
  T = 1.01
)

L_prem_hierar_gumbel_mle <- generiraj_hierarhicno_odvisnost(drevo, funk_porazdelitve, relativne_utezi, parametri_gumbel_mle, 'Gumbel' , d)
SCR_hierar_gumbel_mle <- quantile(L_prem_hierar_gumbel_mle, probs = 0.995) - mean(L_prem_hierar_gumbel_mle) # 0.6327608 
SCR_hierar_gumbel_mle

L_prem_hierar_gumbel_itau <- generiraj_hierarhicno_odvisnost(drevo, funk_porazdelitve, relativne_utezi, parametri_gumbel_itau, 'Gumbel' , d)
SCR_hierar_gumbel_itau <- quantile(L_prem_hierar_gumbel_itau, probs = 0.995) - mean(L_prem_hierar_gumbel_itau) # 0.6073232 
SCR_hierar_gumbel_itau


#--------------------- Clayton prezivetvena kopula -----------------------------

# parametri določeni z MLE metodo
param_clayton_S1_mle <- fitCopula(rotCopula(claytonCopula()), U_podatki_CR_hierar[, c("AO", "DMZ")], method = "ml", lower = 0.001) # 1.286
param_clayton_S2_mle <- fitCopula(rotCopula(claytonCopula()), U_podatki_CR_hierar[, c("S1", "ZP")], method = "ml", lower = 0.001) # 0.9744 
param_clayton_S3_mle <- fitCopula(rotCopula(claytonCopula()), U_podatki_CR_hierar[, c("S2", "SZO")], method = "ml", lower = 0.001) # 0.001
param_clayton_T1_mle <- fitCopula(rotCopula(claytonCopula()), U_podatki_CR_hierar[, c("S3", "POZ")], method = "ml", lower = 0.001) # 0.02396

# parametri določeni z itau metodo
param_clayton_S1_itau <- fitCopula(rotCopula(claytonCopula()), U_podatki_CR_hierar[, c("AO", "DMZ")], method = "itau", lower = 0.001) # 1.286
param_clayton_S2_itau <- fitCopula(rotCopula(claytonCopula()), U_podatki_CR_hierar[, c("S1", "ZP")], method = "itau", lower = 0.001) # 0.5066 
param_clayton_S3_itau <- fitCopula(rotCopula(claytonCopula()), U_podatki_CR_hierar[, c("S2", "SZO")], method = "itau", lower = 0.001) # 0.001
param_clayton_T_itau <- fitCopula(rotCopula(claytonCopula()), U_podatki_CR_hierar[, c("S3", "POZ")], method = "itau", lower = 0.001) # 0.001

parametri_clayton_mle <- list(
  S1 = 1.286,
  S2 = 0.9744,
  S3= 0.001,
  T = 0.02396
)

parametri_clayton_itau <- list(
  S1 = 1.286,
  S2 = 0.5066,
  S3= 0.001,
  T = 0.001
)

L_prem_hierar_clayton_mle <- generiraj_hierarhicno_odvisnost(drevo, funk_porazdelitve, relativne_utezi, parametri_clayton_mle, 'Clayton_survival' , d)
SCR_hierar_clayton_mle <- quantile(L_prem_hierar_clayton_mle, probs = 0.995) - mean(L_prem_hierar_clayton_mle) # 0.627896
SCR_hierar_clayton_mle

L_prem_hierar_clayton_itau <- generiraj_hierarhicno_odvisnost(drevo, funk_porazdelitve, relativne_utezi, parametri_clayton_itau, 'Clayton_survival' , d)
SCR_hierar_clayton_itau <- quantile(L_prem_hierar_clayton_itau, probs = 0.995) - mean(L_prem_hierar_clayton_itau) # 0.6047561
SCR_hierar_clayton_itau

# ------------------------------------------------------------------------------
# ----------------------------- Trtne kopule -----------------------------------
# ------------------------------------------------------------------------------

U_podatki_CR_trta <- pobs(podatki_CR_num)

#-------------------------------- C-trta ---------------------------------------

# Določitev optimalne strukture C-vine
# C-vino izbere vozlišče z največjo vsoto absolutnih Kendallovih $\tau$ kot koren prvega drevesa in potem nadaljuje po pravilih C-vine strukture.
c_trta_mle <- RVineStructureSelect(
  data = U_podatki_CR_trta,
  familyset = c(1, 2, 3, 13, 23, 33, 4, 14, 24, 34),   
  type = 1,         # 1 = C-vine, 2 = D-vine, 0 = R-vine
  selectioncrit = "AIC", # ali "BIC", lahko tudi "loglik"
  method = 'mle',
  treecrit = "tau"
)

c_trta_tau <- RVineStructureSelect(
  data = U_podatki_CR_trta,
  familyset = c(1, 2, 3, 13, 23, 33, 4, 14, 24, 34),   
  type = 1,         # 1 = C-vine, 2 = D-vine, 0 = R-vine
  selectioncrit = "AIC", # ali "BIC", lahko tudi "loglik"
  method = 'itau',
  treecrit = "tau"
)

summary(c_trta_mle)      # pokaže strukturo, izbrane družine in parametre
c_trta_mle$Matrix        # matrika strukture trte 
c_trta_mle$family        # izbrane družine za posamezne robove
c_trta_mle$par           # parametri

plot(c_trta_mle)
plot(c_trta_tau)


C_trtna_kopula_mle <- RVineSim(d, c_trta_mle)
C_trtna_kopula_mle <- t(C_trtna_kopula_mle)

C_trtna_kopula_mle_x <- matrix(NA, nrow = n, ncol = d)
for (i in 1:n) {
  C_trtna_kopula_mle_x[i, ] <- inv_porazdelitve[[i]](C_trtna_kopula_mle[i, ])
}

L_C_trta_mle <- apply(t(C_trtna_kopula_mle_x) %*% diag(segm_UT), 1 , sum)
SCR_C_trta_mle <- quantile(L_C_trta_mle, probs = 0.995) - mean(L_C_trta_mle) # 45.50248
SCR_C_trta_mle


C_trtna_kopula_itau <- RVineSim(d, c_trta_tau)
C_trtna_kopula_itau <- t(C_trtna_kopula_itau)

C_trtna_kopula_itau_x <- matrix(NA, nrow = n, ncol = d)
for (i in 1:n) {
  C_trtna_kopula_itau_x[i, ] <- inv_porazdelitve[[i]](C_trtna_kopula_itau[i, ])
}

L_C_trta_itau <- apply(t(C_trtna_kopula_itau_x) %*% diag(segm_UT), 1 , sum)
SCR_C_trta_itau <- quantile(L_C_trta_itau, probs = 0.995) - mean(L_C_trta_itau) # 57.52312 
SCR_C_trta_itau

#------------------------------- D-trta ----------------------------------------

matrika_kendallovih_tau <- cor(podatki_CR_num, method = "kendall")
matrika_utezi_poti <- 1 - abs(matrika_kendallovih_tau) #ker funkcija solve_TSP minimizira, mi pa želimo maksimizirat, obrnemo uteži
tsp <- TSP(matrika_utezi_poti)
pot <- solve_TSP(tsp, method = "nn")
as.integer(pot) # 3 4 5 2 1

D_trtna_matrika_1 <- matrix(c(
  2, 2, 1, 5, 4,
  0, 1, 2, 1, 5, 
  0, 0, 5, 2, 1,
  0, 0, 0, 4, 2,
  0, 0, 0, 0, 3), nrow = 5, ncol = 5, byrow = TRUE)

D_trta_mle <- RVineCopSelect(
  data = U_podatki_CR_trta,
  Matrix  = D_trtna_matrika_1,
  familyset = c(1, 2, 3, 13, 23, 33, 4, 14, 24, 34), # iste družine kot prej
  selectioncrit = "AIC",
  method = "mle"
)


D_trta_itau <- RVineCopSelect(
  data = U_podatki_CR_trta,
  Matrix  = D_trtna_matrika_1,
  familyset = c(1, 2, 3, 13, 23, 33, 4, 14, 24, 34), # iste družine kot prej
  selectioncrit = "AIC",
  method = "itau"
)

plot(D_trta_mle)
plot(D_trta_itau)


D_trtna_kopula_mle <- RVineSim(d, D_trta_mle)
D_trtna_kopula_mle <- t(D_trtna_kopula_mle)

D_trtna_kopula_mle_x <- matrix(NA, nrow = n, ncol = d)
for (i in 1:n) {
  D_trtna_kopula_mle_x[i, ] <- inv_porazdelitve[[i]](D_trtna_kopula_mle[i, ])
}

L_D_trta_mle <- apply(t(D_trtna_kopula_mle_x) %*% diag(segm_UT), 1 , sum) 
SCR_D_trta_mle <- quantile(L_D_trta_mle, probs = 0.995) - mean(L_D_trta_mle) # 50.67594  
SCR_D_trta_mle


D_trtna_kopula_itau <- RVineSim(d, D_trta_itau)
D_trtna_kopula_itau <- t(D_trtna_kopula_itau)

D_trtna_kopula_itau_x <- matrix(NA, nrow = n, ncol = d)
for (i in 1:n) {
  D_trtna_kopula_itau_x[i, ] <- inv_porazdelitve[[i]](D_trtna_kopula_itau[i, ])
}

L_D_trta_itau <- apply(t(D_trtna_kopula_itau_x) %*% diag(segm_UT), 1 , sum) 
SCR_D_trta_itau <- quantile(L_D_trta_itau, probs = 0.995) - mean(L_D_trta_itau) # 54.51171 
SCR_D_trta_itau

######################    REZULTATI      ##############################

# podatki za empirični kombiniran količnik 
x_emp <- sort(L_prem_emp)
y_emp <- ecdf(L_prem_emp )(x_emp)
empiricni_podatki <- data.frame(x_vals = x_emp, emp_vals = y_emp)

# simulirani podatki s kopulami
L_skupaj <- cbind(L_prem_neodv, L_prem_norm, 
                  L_prem_hierar_gumbel_mle * 100, L_prem_hierar_gumbel_itau * 100, L_prem_hierar_clayton_mle * 100, L_prem_hierar_clayton_itau * 100, 
                  L_C_trta_mle, L_C_trta_itau, L_D_trta_mle, L_D_trta_itau)
colnames(L_skupaj) <- c("L_prem_neodv", "L_prem_norm", 
                        "L_prem_Gumbel_MLE", "L_prem_Gumbel_itau", "L_prem_Clayton_MLE", "L_prem_Clayton_itau",
                        "L_prem_C_MLE", "L_prem_C_itau", "L_prem_D_MLE", "L_prem_D_itau")

# deskriptivna statistika
skim(L_skupaj)
print(describe(L_skupaj))

# simulirani SCR_P
SCR_P_skupaj <- cbind(SCR_neodv, SCR_norm,
                      SCR_hierar_gumbel_mle * 100, SCR_hierar_gumbel_itau * 100, SCR_hierar_clayton_mle * 100, SCR_hierar_clayton_itau * 100,
                      SCR_C_trta_mle, SCR_C_trta_itau, SCR_D_trta_mle, SCR_D_trta_itau)
colnames(SCR_P_skupaj) <- c("SCR_neodv", "SCR_norm", 
                        "SCR_hierar_gumbel_mle", "SCR_hierar_gumbel_itau", "SCR_hierar_clayton_mle", "SCR_hierar_clayton_itau",
                        "SCR_C_trta_mle", "SCR_C_trta_itau", "SCR_D_trta_mle", "SCR_D_trta_itau")
SCR_P_skupaj
