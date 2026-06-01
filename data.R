df <- read.csv("E:/ISEN 2025-2026/projet BigDAta_IA_Web/travail/IRVE.csv")

library(dplyr)
library(tidyr)

# 1. Suppression des colonnes inutiles ou trop incomplètes
df_clean <- df %>% 
  select(-observations, -num_pdl, -raccordement, -cable_t2_attache)

df_clean <- df_clean %>%
  select(-siren_amenageur)

df_clean <- df_clean %>%
  select(-consolidated_code_postal)

# 2. Remplacement des NA pour le nom de l'opérateur
df_clean <- df_clean %>% 
  mutate(nom_operateur = replace_na(nom_operateur, "Inconnu"))

# 3. Harmonisation des variables Oui / Non / Non renseigné
df_clean <- df_clean %>% 
  mutate(
    gratuit = case_when(
      gratuit %in% c("True", "true", "TRUE", "1") ~ "Oui",
      gratuit %in% c("False", "false", "FALSE", "0") ~ "Non",
      is.na(gratuit) ~ "Non renseigne",
      TRUE ~ gratuit
    ),
    paiement_cb = case_when(
      paiement_cb %in% c("True", "true", "TRUE", "1") ~ "Oui",
      paiement_cb %in% c("False", "false", "FALSE", "0") ~ "Non",
      is.na(paiement_cb) ~ "Non renseigne",
      TRUE ~ paiement_cb
    ),
    paiement_autre = case_when(
      paiement_autre %in% c("True", "true", "TRUE", "1") ~ "Oui",
      paiement_autre %in% c("False", "false", "FALSE", "0") ~ "Non",
      is.na(paiement_autre) ~ "Non renseigne",
      TRUE ~ paiement_autre
    ),
    reservation = case_when(
      reservation %in% c("True", "true", "TRUE", "1") ~ "Oui",
      reservation %in% c("False", "false", "FALSE", "0") ~ "Non",
      TRUE ~ reservation
    )
  )

# 4. Subset pour l'évolution temporelle
df_temporel <- df_clean %>% 
  filter(!is.na(date_mise_en_service))

# 5. Subset pour la carte
df_carte <- df_clean %>%
  filter(
    consolidated_is_lon_lat_correct == 'True',
    !is.na(consolidated_longitude),
    !is.na(consolidated_latitude)
  )

# 6. Subset pour les analyses sur la puissance
df_puissance <- df_clean %>%
  filter(
    puissance_nominale > 0,
    puissance_nominale <= 400
  )

# 7. Subset pour l'analyse de la tarification
df_regression_tarifs <- df_clean %>% 
  filter(!is.na(tarification))

# 8. Vérification finale des valeurs manquantes
colSums(is.na(df_clean))

