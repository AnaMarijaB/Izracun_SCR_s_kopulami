############################      PODATKI      #################################

# Priloga: Tabela 14
# izhidiščni kombinirani količniki po segmentih, letih in zavarovalnicah, izraženi v odstotkih 
podatki_tabela_CR <- podatki_CR %>% 
  dplyr::select(Leto, Zavarovalnica, AO, DMZ, POZ, SZO, ZP) %>%
  mutate(Leto = as.character(Leto),
         across(c(AO, DMZ, POZ, SZO, ZP),
                ~ {val <- .x * 100
                val <- round(val, 1)
                val_text <- format(val, nsmall = 1)
                gsub("\\.", ",", val_text)})
  )%>%
  arrange(Leto, Zavarovalnica)

podatki_tabela_CR[1:dim(podatki_tabela_CR)[1] %% 6 != 1, 1] <- "" # Leto v tabeli izpisemo le prvic

print(xtable(podatki_tabela_CR), type = "latex")

# Priloga: Tabela 15
# izhidiščne premije za izračun uteži po segmentih, letih in zavarovalnicah (v 1000 eur)
podatki_tabela_PREM <- podatki_PREM %>%
  select(Leto, Zavarovalnica, AO, DMZ, POZ, SZO, ZP) %>%
  mutate(
    Leto = as.character(Leto),
    across(c(AO, DMZ, POZ, SZO, ZP),
           ~ formatC(.x, digits = 0, format = "f", big.mark = ".")
    )
  ) %>%
  arrange(Leto, Zavarovalnica)

podatki_tabela_PREM[1:dim(podatki_tabela_PREM)[1] %% 6 != 1, 1] <- "" # Leto v tabeli izpisemo le prvic

print(xtable(podatki_tabela_PREM), type = "latex")

# Slika 12
# izhodiščni kombinirani količniki po segentih in letih, izraženi v odstotkih
podatki_graf_leto_1 <- podatki_PREM %>% 
  dplyr::select(Leto, AO, DMZ, POZ, SZO, ZP) %>%
  mutate(Leto = as.character(Leto)) %>%
  group_by(Leto) %>%
  summarize(AO = sum(AO), DMZ = sum(DMZ), POZ = sum(POZ), SZO = sum(SZO), ZP = sum(ZP)) 

podatki_graf_leto_2 <- podatki_OB %>% 
  dplyr::select(Leto, AO, DMZ, POZ, SZO, ZP) %>%
  mutate(Leto = as.character(Leto)) %>%
  group_by(Leto) %>%
  summarize(AO = sum(AO), DMZ = sum(DMZ), POZ = sum(POZ), SZO = sum(SZO), ZP = sum(ZP)) 

podatki_graf_leto_2[, 2:6] <- podatki_graf_leto_2[, 2:6]/podatki_graf_leto_1[, 2:6] 
podatki_graf_leto <- podatki_graf_leto_2 %>% 
  tidyr::pivot_longer(cols = c(AO, DMZ, POZ, SZO, ZP), names_to = "Segment", values_to = "Vrednost") %>%
  mutate(Vrednost = round(Vrednost * 100, 1))

ggplot(podatki_graf_leto, aes(x = Leto, y = Vrednost, fill = Segment)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  geom_text(aes(label = scales::label_number(accuracy = 0.1, decimal.mark = ",")(Vrednost)), 
            position = position_dodge(width = 0.8), 
            vjust = -0.3, size = 3) +  
  scale_fill_brewer(palette = "Set2") +
  labs(x = "Leto",
       y = "Kombiniran količnik (%)",
       fill = "Segment") +
  theme_minimal() + 
  theme(legend.position = "bottom",
        legend.box = "horizontal")

# Slika 13
# izhodiščni kombinirani količniki po segentih in zavarovalnicah, izraženi v odstotkih
podatki_graf_zav_1 <- podatki_PREM %>% 
  dplyr::select(Zavarovalnica, AO, DMZ, POZ, SZO, ZP) %>%
  group_by(Zavarovalnica) %>%
  summarize(AO = sum(AO), DMZ = sum(DMZ), POZ = sum(POZ), SZO = sum(SZO), ZP = sum(ZP)) 

podatki_graf_zav_2 <- podatki_OB %>% 
  dplyr::select(Zavarovalnica, AO, DMZ, POZ, SZO, ZP) %>%
  group_by(Zavarovalnica) %>%
  summarize(AO = sum(AO), DMZ = sum(DMZ), POZ = sum(POZ), SZO = sum(SZO), ZP = sum(ZP)) 

podatki_graf_zav_2[, 2:6] <- podatki_graf_zav_2[, 2:6]/podatki_graf_zav_1[, 2:6] 
podatki_graf_zav <- podatki_graf_zav_2 %>% 
  tidyr::pivot_longer(cols = c(AO, DMZ, POZ, SZO, ZP), names_to = "Segment", values_to = "Vrednost") %>%
  mutate(Vrednost = round(Vrednost * 100, 1))

ggplot(podatki_graf_zav, aes(x = Zavarovalnica, y = Vrednost, fill = Segment)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  geom_text(aes(label = scales::label_number(accuracy = 0.1, decimal.mark = ",")(Vrednost)), 
            position = position_dodge(width = 0.8), 
            vjust = -0.3, size = 3) +  
  scale_fill_brewer(palette = "Set2") +
  labs(x = "Zavarovalnica",
       y = "Kombiniran količnik (%)",
       fill = "Segment") +
  theme_minimal() + 
  theme(legend.position = "bottom",
        legend.box = "horizontal")

# Slika 15
# Porazdelitev empiričnega kombiniranega količnika celotnega portfelja
x_emp <- sort(L_prem_emp)
y_emp <- ecdf(L_prem_emp)(x_emp)
empiricni_podatki <- data.frame(x_vals = x_emp, emp_vals = y_emp)

ggplot() +
  geom_step(data = empiricni_podatki, aes(x = x_vals, y = emp_vals, color = "Empiricni podatki"), linewidth = 0.7, show.legend = FALSE) +
  geom_hline(yintercept = c(0, 1), linetype = "dashed", color = "gray") +
  labs(x = "Kombiniran količnik (v %)", y = "Verjetnost") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white", colour = "grey50")) +
  scale_color_manual(values = c("Empiricni podatki" = "blue"))


######################    ROBNE PORAZDELITVE      ##############################

# Slika 16
# Grafi porazdelitvenih funkcij škodnih količnikov
grafi_porazdelitev <- list()

for (i in 1:5) {
  podatki <- podatki_CR_num[, i]
  ime_segmenta <- colnames(podatki_CR_num)[i]
  mu <- mu_log_vec[i]
  sigma <- sd_log_vec[i]
  
  x_emp <- sort(podatki)
  y_emp <- ecdf(podatki)(x_emp)
  empiricni_podatki <- data.frame(x_vals = x_emp, emp_vals = y_emp)
  
  x_teo <- seq(min(podatki), max(podatki) + 0.25, by = 0.01)
  y_teo <- plnorm(x_teo, meanlog = mu, sdlog = sigma)
  teoreticni_podatki <- data.frame(x_vals = x_teo, theo_vals = y_teo)
  
  # graf
  grafi_porazdelitev[[i]] <- ggplot() +
    geom_step(data = empiricni_podatki, aes(x = x_vals, y = emp_vals), color = "blue") +
    geom_line(data = teoreticni_podatki, aes(x = x_vals, y = theo_vals), color = "red3") +
    geom_hline(yintercept = c(0, 1), linetype = "dashed", color = "gray") +
    scale_color_manual(values = c("Empiricni podatki" = "blue", 
                                  "Teoreticni podatki" = "red")) +
    labs(title = paste(ime_segmenta),
         x = 'CR',
         y = 'Verjetnost') +
    theme_minimal() +
    theme(panel.grid = element_blank(),
          panel.background = element_rect(fill = "white", colour = "grey50"),
          plot.title = element_text(hjust = 0.4),
          legend.position = "none")
}

zdruzeni_graf <- patchwork::wrap_plots(grafi_porazdelitev, nrow = 1)
print(zdruzeni_graf)

###########################    REZULTATI      ##################################

# Slika 21
# Porazdelitvene funkcije simuliranih kombiniranih količnikov celotnega portfelja
ggplot() +
  geom_step(data = empiricni_podatki, aes(x = x_vals, y = emp_vals, color = "Empiricni podatki"), linewidth = 1.2) +
  stat_ecdf(data = L_skupaj, aes(x = L_prem_neodv, color = "Neodv"), geom = "step", size = 0.9) +
  stat_ecdf(data = L_skupaj, aes(x = L_prem_norm, color = "Norm"), geom = "step", size = 0.9) +
  stat_ecdf(data = L_skupaj, aes(x = L_prem_Gumbel_MLE, color = "Gumbel_MLE"), geom = "step", size =0.9) +
  stat_ecdf(data = L_skupaj, aes(x = L_prem_Gumbel_itau, color = "Gumbel_itau"), geom = "step", size = 0.9) +
  stat_ecdf(data = L_skupaj, aes(x = L_prem_Clayton_MLE, color = "Clayton_MLE"), geom = "step", size = 0.9) +
  stat_ecdf(data = L_skupaj, aes(x = L_prem_Clayton_itau, color = "Clayton_itau"), geom = "step", size = 0.9) +
  stat_ecdf(data = L_skupaj, aes(x = L_prem_C_MLE, color = "C_MLE"), geom = "step", size = 0.9) +
  stat_ecdf(data = L_skupaj, aes(x = L_prem_C_itau, color = "C_itau"), geom = "step", size = 0.9) +
  stat_ecdf(data = L_skupaj, aes(x = L_prem_D_MLE, color = "D_MLE"), geom = "step", size = 0.9) +
  stat_ecdf(data = L_skupaj, aes(x = L_prem_D_itau, color = "D_itau"), geom = "step", size = 0.9) +
  geom_hline(yintercept = c(0, 1), linetype = "dashed", color = "gray") +
  scale_color_brewer(palette = "Spectral", name = "Metoda") + 
  labs(x = "Kombiniran količnik (v %)", y = "Verjetnost") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white", colour = "grey50")) +
  guides(color = guide_legend(override.aes = list(size = 5))) 

