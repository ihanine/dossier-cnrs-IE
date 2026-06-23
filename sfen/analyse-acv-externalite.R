# =============================================================================
# Analyse des externalités — ACV de l'électricité
# -----------------------------------------------------------------------------
# Objet   : croiser les évaluations monétaires des externalités (16 indicateurs)
#           avec l'ACV d'EDF, produire les statistiques et les graphiques de coût
#           des externalités au MWh, et tracer les chroniques de production.
# Entrées : data_unece_acv.xlsx          (feuille "clean_data_price")
#           acv_edf_cout_externalite.xlsx (feuille "clean_data_edf")
#           FR 2018-2021.csv             (chroniques horaires, Electricity Maps)
# Sorties : statistiques_methodes.xlsx   + graphiques
# Auteur  : Ilyas Hanine
# =============================================================================

# ---- 1. Dépendances et configuration ----------------------------------------

library(readxl)
library(writexl)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(viridis)

# Chemins des fichiers (à adapter)
paths <- list(
  price = "data_unece_acv.xlsx",
  edf   = "acv_edf_cout_externalite.xlsx",
  hourly = "FR 2018-2021.csv"
)

# Méthodes d'évaluation retenues pour le prix moyen des externalités
methodes_prix <- c("Coût des dommages", "Prix de marché")

# ---- 2. Chargement et nettoyage des prix des externalités --------------------

data_price <- read_excel(paths$price, sheet = "clean_data_price") |>
  mutate(Indicateur = str_replace_all(Indicateur, "\\(€/", "("))

# ---- 3. Externalités issues de l'ACV d'EDF -----------------------------------

data_edf <- read_excel(paths$edf, sheet = "clean_data_edf")

# Coût des externalités au MWh = prix unitaire x quantité d'impact (x 1000)
data_edf_ext <- inner_join(
  data_price, data_edf,
  by = "Indicateur", suffix = c("_prix", "_acv")
) |>
  mutate(cout_mwh = 1000 * Valeur_prix * Valeur_acv) |>
  na.omit()

# ---- 4. Statistiques descriptives --------------------------------------------

# Sur les évaluations monétaires brutes (méthodes avec au moins 2 évaluations)
stat_price <- data_price |>
  group_by(Indicateur, Méthode) |>
  summarise(
    n      = n(),
    mean   = mean(Valeur, na.rm = TRUE),
    sd     = sd(Valeur, na.rm = TRUE),
    median = median(Valeur, na.rm = TRUE),
    .groups = "drop"
  ) |>
  filter(n > 1)

# Sur les externalités de l'ACV d'EDF
stat_edf <- data_edf_ext |>
  group_by(Indicateur, Méthode) |>
  summarise(
    n      = n(),
    mean   = mean(cout_mwh, na.rm = TRUE),
    sd     = sd(cout_mwh, na.rm = TRUE),
    median = median(cout_mwh, na.rm = TRUE),
    .groups = "drop"
  ) |>
  filter(n > 1)

# ---- 5. Graphiques du coût des externalités ----------------------------------

# Médiane par indicateur et par méthode (ACV EDF)
ggplot(stat_edf, aes(x = median, y = Indicateur, colour = Méthode)) +
  geom_point(size = 4) +
  labs(
    title = "Coût des externalités du kWh nucléaire d'EDF (médiane des évaluations)",
    subtitle = "Sources : ACV EDF 2022, Amadei et al. 2021",
    x = "€/MWh", y = "Indicateur"
  )

# Nuage complet des coûts au MWh (ACV EDF)
ggplot(data_edf_ext, aes(x = Indicateur, y = cout_mwh, colour = Méthode)) +
  geom_point(size = 2.5) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(
    title = "Coût des externalités d'après l'ACV EDF (16 articles de revue)",
    x = "Indicateurs d'ACV", y = "€/MWh", colour = "Méthode employée"
  )

# Évaluations brutes (Amadei et al. 2021)
ggplot(data_price, aes(x = Indicateur, y = Valeur, colour = Méthode)) +
  geom_point(size = 2.5) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(
    title = "Coût des externalités d'après Amadei et al. 2021",
    x = "Indicateurs d'ACV", y = "€/[unité]", colour = "Méthode employée"
  )

# ---- 6. Couverture des indicateurs par méthode -------------------------------

# Comptage indicateur x méthode (remplace les doubles boucles d'origine)
methode_counts <- data_price |>
  count(Indicateur, Méthode, name = "n")

# Tableau large : une colonne par méthode
methode_table <- methode_counts |>
  pivot_wider(names_from = Méthode, values_from = n, values_fill = 0)

# Histogramme groupé
ggplot(methode_counts, aes(x = Indicateur, y = n, fill = Méthode)) +
  geom_col(position = "dodge") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(x = "Indicateur", y = "Nombre d'évaluations", fill = "Méthode")

# Export (attention : écrase le fichier existant)
write_xlsx(methode_table, "statistiques_methodes.xlsx")

# Normalisation des valeurs par indicateur (max = 1), pour comparaisons relatives
data_price_norm <- data_price |>
  group_by(Indicateur) |>
  mutate(Valeur = Valeur / max(Valeur, na.rm = TRUE)) |>
  ungroup()

# ---- 7. Prix moyen des externalités (comparaison filières UNECE) -------------

mean_price <- data_price |>
  filter(Méthode %in% methodes_prix) |>
  group_by(Indicateur) |>
  summarise(prix_moyen = mean(Valeur, na.rm = TRUE), .groups = "drop")

# (Comparaison des filières d'électricité avec les données UNECE : à compléter.)

# ---- 8. Chroniques horaires de production (Electricity Maps) ------------------

# ymd_hms gère directement le format ISO 8601 avec décalage horaire :
# plus besoin de découper la chaîne à la main.
df_fr <- read.csv(paths$hourly) |>
  mutate(datetime = ymd_hms(datetime))

prod_long <- df_fr |>
  transmute(
    time       = datetime,
    nuclear    = power_production_nuclear_avg,
    hydro      = power_production_hydro_avg,
    solar      = power_production_solar_avg,
    wind       = power_production_wind_avg,
    gas        = power_production_gas_avg,
    coal       = power_production_coal_avg,
    biomass    = power_production_biomass_avg,
    geothermal = power_production_geothermal_avg
  ) |>
  pivot_longer(
    -time, names_to = "technology", values_to = "production"
  )

ggplot(prod_long, aes(x = time, y = production, fill = technology)) +
  geom_area() +
  scale_fill_viridis(discrete = TRUE) +
  labs(
    title = "Chroniques horaires de production du parc électrique français, 2018-2021",
    x = NULL, y = "Production", fill = "Filière"
  )