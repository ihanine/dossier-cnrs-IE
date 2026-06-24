# =============================================================================
# Violences sexistes et sexuelles en milieu étudiant — analyse quantitative
# EHESS, 2024-2025
#
# Notes de réécriture :
#   - chargement des packages regroupé en tête ;
#   - logique dupliquée (diagrammes alluviaux femmes/hommes, tableaux de Likert
#     hommes/non-hommes) factorisée en fonctions ;
#   - code mort ou non fonctionnel retiré (brouillon networkD3 à la syntaxe
#     incorrecte, références à un objet `my_data` inexistant, lignes de notes
#     non commentées qui interrompaient l'exécution) ;
#   - noms de colonnes du jeu de données conservés tels quels (age, sexe,
#     genre_regroupe, orientation_politique, vss_*, licker_*, etc.) pour rester
#     compatible avec `vss_tab.xls` ;
#   - les choix d'analyse (recodage politique, barème de Likert) sont préservés
#     à l'identique et signalés par un commentaire là où ils méritent discussion.
# =============================================================================

library(readxl)
library(tidyverse)   # dplyr, ggplot2, tidyr, forcats, purrr, stringr...
library(scales)
library(gridExtra)
library(ggalluvial)
library(ggtext)      # titres en Markdown/italique


# =============================================================================
# 1. Import et préparation des données
# =============================================================================

# Attention : la variable `virginite` se lit "à l'envers" (Non = vierge,
# Oui = non-vierge), cf. recodage explicite plus bas.
data <- read_excel("vss_tab.xls")

# Identifiant unique par enquêté·e
data <- data %>% mutate(ID = row_number())

# Mise au bon format des variables numériques
data <- data %>%
  mutate(
    age                  = as.numeric(age),
    orientation_politique = as.numeric(orientation_politique)
  )


# =============================================================================
# 2. Recodage de l'orientation politique
# =============================================================================

data <- data %>%
  mutate(orientation_politique_str = case_when(
    orientation_politique %in% c(1, 2)  ~ "Extrême gauche",
    orientation_politique %in% c(3, 4)  ~ "Gauche",
    orientation_politique %in% c(5, 6)  ~ "Centre",
    orientation_politique %in% c(7, 8)  ~ "Droite",
    orientation_politique %in% c(9, 10) ~ "Extrême droite",
    TRUE ~ NA_character_
  ))

# Contrôles de cohérence du recodage
print("Distribution des orientations politiques recodées :")
print(table(data$orientation_politique_str, useNA = "always"))

print("Correspondance ancien / nouveau codage :")
print(table(data$orientation_politique, data$orientation_politique_str,
            useNA = "always"))


# =============================================================================
# 3. Scores des échelles de Likert
# =============================================================================

variables_licker <- c(
  "licker_besoinsex_homme",   "licker_pulsionsex_homme",
  "licker_zeroviol_homme",    "licker_refussexe_homme",
  "licker_frapper_agresseur", "licker_viol_cinema",
  "licker_vss_premedite",     "licker_legitimite_delais",
  "licker_viol_croyance",     "licker_adaptation_desirsexe",
  "licker_humiliation_femme", "licker_prostitue_viol",
  "licker_drague_homme",      "licker_mensonge_femme",
  "licker_detressehomme_femme"
)

# Barème conservé tel quel : l'écart 2 -> 4 (sans 3) accentue volontairement
# l'opposition accord / désaccord ; "Ne sais pas" est codé 0.
convert_to_score <- function(x) {
  case_when(
    x == "Pas du tout d'accord" ~ 1,
    x == "Plutôt pas d'accord"  ~ 2,
    x == "Plutôt d'accord"      ~ 4,
    x == "Tout à fait d'accord" ~ 5,
    x == "Ne sais pas"          ~ 0,
    TRUE ~ NA_real_
  )
}

# Création des colonnes "<variable>_score"
data <- data %>%
  mutate(across(all_of(variables_licker), convert_to_score,
                .names = "{.col}_score"))


# =============================================================================
# 4. Distribution de l'orientation politique (contrôle visuel)
# =============================================================================

data %>%
  count(orientation_politique) %>%
  mutate(orientation_politique = factor(orientation_politique)) %>%
  ggplot(aes(x = orientation_politique, y = n)) +
  geom_col(fill = "#2171b5") +
  labs(title = "Distribution de l'orientation politique de l'échantillon",
       x = "1 (gauche) — 10 (droite)", y = "Effectif") +
  theme_minimal()


# =============================================================================
# 5. Diagrammes alluviaux des VSS subies, par genre
# =============================================================================

# Six formes de VSS, dans l'ordre d'affichage (de la moins à la plus engageante
# physiquement). On exclut `vss_zero` (aucune VSS) et `vss_abstention`.
variables_sankey <- c(
  "vss_harcelement_numerique", "vss_harcelement_sexuel",
  "vss_baiser", "vss_attouchement",
  "vss_fellation", "vss_penetration"
)

labels_sankey <- c(
  "...du harcèlement numérique à caractère sexuel ?",
  "...du harcèlement sexuel ?",
  "...des baisers (sur la bouche ou sur la joue) ?",
  "...des attouchements ?",
  "...une fellation ou cunnilingus reçu(e)s ou effectué(e)s ?",
  "...une pénétration vaginale ou anale reçues ou effectuées ?"
)

# Construit le format long puis trace le diagramme alluvial pour un sous-groupe.
# On ne garde que les répondant·es qui n'ont coché ni "aucune VSS" ni
# "ne souhaite pas répondre" (vss_zero et vss_abstention tous deux manquants).
# Une réponse "X" vaut "Oui", toute autre valeur (y compris NA) vaut "Non".
plot_alluvial_vss <- function(df, titre) {
  df_long <- df %>%
    filter(is.na(vss_zero), is.na(vss_abstention)) %>%
    select(ID, all_of(variables_sankey)) %>%
    pivot_longer(all_of(variables_sankey),
                 names_to = "variable", values_to = "reponse_brute") %>%
    mutate(
      reponse  = if_else(!is.na(reponse_brute) & reponse_brute == "X",
                         "Oui", "Non"),
      question = factor(variable, levels = variables_sankey,
                        labels = labels_sankey),
      ID       = as.character(ID)
    )

  n_rep <- n_distinct(df_long$ID)
  sous_titre <- sprintf(
    paste0("Violences sexistes et sexuelles subies sans consentement ",
           "(n = %d).<br><br><i>Sans votre consentement, avez-vous déjà subi ",
           "au cours de votre vie...</i>"),
    n_rep
  )

  ggplot(df_long,
         aes(x = question, stratum = reponse, alluvium = ID,
             fill = reponse, label = reponse)) +
    scale_fill_brewer(type = "qual", palette = "Set2") +
    geom_flow(stat = "alluvium", lode.guidance = "rightleft",
              color = "darkgray", alpha = 0.7) +
    geom_stratum(alpha = 0.9) +
    theme_minimal() +
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title  = element_markdown(size = 11)) +
    labs(title = paste0(titre, "<br>", sous_titre),
         fill = "Réponse", x = NULL)
}

# Non-hommes (femmes et "autre")
plot_alluvial_vss(
  subset(data, sexe != "Un homme"),
  "Enquêté·es non hommes"
)

# Hommes
plot_alluvial_vss(
  subset(data, sexe == "Un homme"),
  "Enquêtés hommes"
)


# =============================================================================
# 6. Prévalence des VSS selon le statut de virginité
# =============================================================================

vss_types <- c("vss_penetration", "vss_fellation", "vss_attouchement",
               "vss_baiser", "vss_harcelement_numerique", "vss_harcelement_sexuel")

couleurs_virginite <- c(
  "Personnes vierges"     = "#2171b5",
  "Personnes non-vierges" = "#6baed6"
)

# Rappel : virginite "Non" = vierge, "Oui" = non-vierge.
vss_long <- data %>%
  select(virginite, all_of(vss_types)) %>%
  pivot_longer(all_of(vss_types), names_to = "type_vss", values_to = "has_vss") %>%
  group_by(virginite, type_vss) %>%
  summarise(
    n          = sum(has_vss == "X", na.rm = TRUE),
    total      = n(),
    proportion = n / total,
    .groups = "drop"
  ) %>%
  mutate(
    type_vss = factor(type_vss, levels = vss_types,
                      labels = c("Pénétration\nforcée", "Fellation\nforcée",
                                 "Attouchement", "Baiser\nforcé",
                                 "Harcèlement\nnumérique", "Harcèlement\nsexuel")),
    virginite = factor(virginite, levels = c("Non", "Oui"),
                       labels = names(couleurs_virginite))
  )

# Graphique "lollipop" : prévalence par type de VSS
ggplot(vss_long, aes(x = proportion, y = type_vss, color = virginite)) +
  geom_segment(aes(x = 0, xend = proportion, yend = type_vss),
               linewidth = 1.3, alpha = 0.5) +
  geom_point(size = 3) +
  scale_x_continuous(labels = percent_format(),
                     limits = c(0, max(vss_long$proportion) * 1.1)) +
  scale_color_manual(values = couleurs_virginite) +
  labs(title = "Prévalence des VSS selon le statut de virginité",
       subtitle = "Comparaison par type de violence",
       x = "Pourcentage de personnes concernées", y = NULL, color = "Statut") +
  theme_minimal() +
  theme(plot.title    = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        panel.grid.major.y = element_blank(),
        legend.position = "bottom")

# Distribution du nombre de VSS subies par personne
vss_count <- data %>%
  mutate(nb_vss = rowSums(across(all_of(vss_types), ~ . == "X"), na.rm = TRUE)) %>%
  count(virginite, nb_vss) %>%
  group_by(virginite) %>%
  mutate(
    proportion = n / sum(n),
    nb_vss     = factor(nb_vss),
    virginite  = factor(virginite, levels = c("Non", "Oui"),
                        labels = names(couleurs_virginite))
  ) %>%
  ungroup()

ggplot(vss_count, aes(x = nb_vss, y = proportion, fill = virginite)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = couleurs_virginite) +
  labs(title = "Distribution du nombre de VSS subies",
       subtitle = "Comparaison selon le statut de virginité",
       x = "Nombre de VSS subies", y = "Pourcentage", fill = "Statut") +
  theme_minimal() +
  theme(plot.title    = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "bottom")


# =============================================================================
# 7. Panorama socio-démographique (4 graphiques)
# =============================================================================

genre_colors <- c(
  "Une femme"                = "#2171b5",
  "Un homme"                 = "#6baed6",
  "Ne souhaite pas répondre" = "#bdd7e7",
  "Autre"                    = "#eff3ff"
)

# 7.1 — Répartition par discipline et par genre
plot1 <- data %>%
  count(discipline, sexe) %>%
  group_by(discipline) %>%
  mutate(total = sum(n)) %>%
  ungroup() %>%
  mutate(discipline = fct_reorder(discipline, total, .desc = TRUE)) %>%
  ggplot(aes(x = discipline, y = n, fill = sexe)) +
  geom_col(position = "stack") +
  scale_fill_manual(values = genre_colors) +
  labs(title = "Répartition des étudiant·e·s par discipline",
       subtitle = "Selon le genre déclaré",
       x = "Discipline", y = "Nombre d'étudiant·e·s", fill = "Genre") +
  theme_minimal() +
  theme(plot.title    = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x   = element_text(angle = 45, hjust = 1))

# 7.2 — Distribution des âges (15-35 ans)
n_exclus <- data %>% filter(age < 15 | age > 35) %>% nrow()

plot2 <- data %>%
  filter(age >= 15, age <= 35) %>%
  ggplot(aes(x = age, fill = sexe)) +
  geom_histogram(position = "dodge", bins = 20) +
  scale_fill_manual(values = genre_colors) +
  labs(title = "Distribution de l'âge des répondant·e·s (15-35 ans)",
       subtitle = sprintf("%d répondant·e·s hors tranche d'âge non représenté·e·s",
                          n_exclus),
       x = "Âge", y = "Nombre d'étudiant·e·s", fill = "Genre") +
  theme_minimal() +
  theme(plot.title    = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5))

# 7.3 — Classe socio-économique déclarée
plot3 <- data %>%
  filter(!is.na(classe_eco)) %>%
  count(classe_eco, sexe) %>%
  group_by(sexe) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  ggplot(aes(x = classe_eco, y = prop, fill = sexe)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = genre_colors) +
  labs(title = "Classe socio-économique déclarée des étudiant·e·s",
       subtitle = "Répartition en pourcentage selon le genre",
       x = "Classe socio-économique", y = "Proportion d'étudiant·e·s",
       fill = "Genre") +
  theme_minimal() +
  theme(plot.title    = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x   = element_text(angle = 45, hjust = 1))

# 7.4 — Orientation sexuelle (en pourcentage)
orientation_counts <- data %>%
  count(orientation) %>%
  arrange(desc(n))

plot4 <- orientation_counts %>%
  mutate(
    prop        = n / sum(n),
    orientation = fct_reorder(orientation, n, .desc = TRUE)
  ) %>%
  ggplot(aes(x = orientation, y = prop, fill = orientation)) +
  geom_col() +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Distribution des orientations sexuelles",
       subtitle = paste("Effectifs totaux :",
                        paste(sprintf("%s : %d", orientation_counts$orientation,
                                      orientation_counts$n),
                              collapse = " | ")),
       x = "Orientation sexuelle", y = "Proportion") +
  theme_minimal() +
  theme(plot.title    = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x   = element_text(angle = 45, hjust = 1),
        legend.position = "none")

grid.arrange(plot1, plot2, plot3, plot4, ncol = 2, nrow = 2)


# =============================================================================
# 8. Scores de Likert moyens par catégorie d'exposition aux VSS
# =============================================================================

# Pour un sous-groupe (selon le genre), classe chaque enquêté·e selon son
# exposition aux VSS, puis renvoie les effectifs et la moyenne de chaque
# échelle de Likert par catégorie.
# L'ordre des conditions est significatif : l'abstention prime, puis
# l'agression physique, puis le harcèlement seul, puis l'absence de VSS.
scores_likert_par_exposition <- function(df) {
  df <- df %>%
    mutate(categorie = case_when(
      vss_abstention == "X" ~ "Ne souhaite pas répondre",
      vss_penetration == "X" | vss_fellation == "X" |
        vss_attouchement == "X" | vss_baiser == "X" ~ "Agression physique",
      vss_harcelement_numerique == "X" |
        vss_harcelement_sexuel == "X" ~ "Harcèlement uniquement",
      vss_zero == "X" ~ "Aucune VSS"
    ))

  ordre_categories <- c("Ne souhaite pas répondre", "Aucune VSS",
                        "Harcèlement uniquement", "Agression physique")

  resume <- df %>%
    group_by(categorie) %>%
    summarise(across(ends_with("_score"), ~ mean(., na.rm = TRUE)),
              n = n(), .groups = "drop") %>%
    arrange(match(categorie, ordre_categories))

  effectifs <- resume %>%
    select(categorie, n) %>%
    pivot_wider(names_from = categorie, values_from = n)

  scores <- resume %>%
    select(-n) %>%
    pivot_longer(-categorie, names_to = "variable", values_to = "score") %>%
    pivot_wider(names_from = categorie, values_from = score)

  list(effectifs = effectifs, scores = scores)
}

# Hommes
res_hommes <- scores_likert_par_exposition(filter(data, genre_regroupe == "Homme"))
print("Hommes — effectifs par catégorie :")
print(res_hommes$effectifs)
print("Hommes — scores moyens par variable et par catégorie :")
print(res_hommes$scores)

# Non-hommes
res_non_hommes <- scores_likert_par_exposition(filter(data, genre_regroupe != "Homme"))
print("Non-hommes — effectifs par catégorie :")
print(res_non_hommes$effectifs)
print("Non-hommes — scores moyens par variable et par catégorie :")
print(res_non_hommes$scores)
