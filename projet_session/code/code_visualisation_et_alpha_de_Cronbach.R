library(tidyverse)
library(psych)

# ===============================
# 1) Code de collecte de données (base investissements)
# ===============================
investissements <- tibble(
  Club = c(
    "Manchester City Group",
    "Newcastle United",
    "Inter Miami",
    "LA Dodgers",
    "Toronto Raptors",
    "AC Milan",
    "PSG",
    "Arsenal"
  ),
  
  Origine = c(
    "Moyen-Orient",
    "Moyen-Orient",
    "États-Unis",
    "États-Unis",
    "Canada",
    "Europe",
    "Moyen-Orient",
    "États-Unis"
  ),
  
  Type_investisseur = c(
    "Fonds souverain",
    "Fonds souverain",
    "Capital privé",
    "Capital privé",
    "Gestion d'actifs",
    "Capital privé",
    "Fonds souverain",
    "Capital privé"
  ),
  
  Investissement_MUSD = c(
    5500, 4200, 1200, 2500, 1800, 1300, 6000, 2000
  ),
  
  Indice_soft_power = c(
    9.5, 9.2, 6.5, 6.8, 6.0, 7.0, 9.8, 6.7
  )
)

# ===============================
# 2) Code du bubble plot final
# ===============================
ggplot(
  investissements,
  aes(
    x = Origine,
    y = Indice_soft_power,
    size = Investissement_MUSD,
    color = Type_investisseur
  )
) +
  geom_point(alpha = 0.7) +
  
  # Axe Y : soft power
  scale_y_continuous(
    limits = c(5, 10),
    breaks = 5:10,
    name = "Indice de soft power (5 à 10)"
  ) +
  
  # Taille des bulles + légende avec 1000, 2500, 5000, 10000
  scale_size_continuous(
    range  = c(5, 25),
    limits = c(1000, 10000),
    breaks = c(1000, 2500, 5000, 10000),
    labels = c("1 000", "2 500", "5 000", "10 000"),
    name   = "Investissement (M USD)"
  ) +
  
  labs(
    title = "Financiarisation et soft power dans le sport professionnel",
    subtitle = "Les investissements étrangers comme instruments économiques et géopolitiques",
    x = "Origine géopolitique de l'investisseur",
    color = "Type d'investisseur"
  ) +
  
  # Légende des couleurs : bulles un peu plus grosses
  guides(
    color = guide_legend(
      override.aes = list(size = 7)
    )
  ) +
  
  theme_minimal(base_family = "serif") +
  
  theme(
    plot.title = element_text(
      face = "bold",
      size = 14,
      margin = margin(b = 10)
    ),
    plot.subtitle = element_text(
      size = 11,
      margin = margin(b = 20)
    ),
    
    axis.title.x = element_text(
      margin = margin(t = 15)
    ),
    
    axis.text.x = element_text(
      angle = 20,
      hjust = 1
    ),
    
    legend.position = "bottom",
    legend.margin = margin(t = 10),
    legend.box.margin = margin(t = 10),
    
    panel.background = element_rect(
      fill = "grey92",
      colour = NA
    ),
    
    plot.background = element_rect(
      fill = "grey95",
      colour = NA
    ),
    
    plot.margin = margin(20, 20, 20, 20)
  )

# ===============================
# 3) Alpha de Cronbach
# ===============================

# --- Données OG des composantes de soft power ---
softpower_data <- data.frame(
  Capacite_softpower_Etat = c(8.5, 7.2, 9.1, 6.8, 8.9, 7.5, 9.3, 6.7),
  Mediatisation = c(70, 55, 95, 40, 85, 60, 100, 45),
  Influence_Franchise_ou_club = c(9.1, 7.8, 8.5, 6.0, 9.7, 8.0, 9.9, 6.5)
)

# --- Fonction de rescoring sur une échelle 0–10 (Min-Max Normalisation) ---
rescale_0_10 <- function(x) {
  (x - min(x)) / (max(x) - min(x)) * 10
}

# --- Application du rescoring ---
softpower_agregat <- softpower_data %>%
  mutate(
    Capacite_softpower_Etat = round(rescale_0_10(Capacite_softpower_Etat), 1),
    Mediatisation = round(rescale_0_10(Mediatisation), 1),
    Influence_Franchise_ou_club = round(rescale_0_10(Influence_Franchise_ou_club), 1)
  )

# --- Calcul de l'alpha de Cronbach sur les données uniformisées ---
alpha_scaled <- psych::alpha(softpower_agregat)

# --- Affichage du résultat ---
alpha_scaled