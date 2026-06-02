df <- read.csv("E:/ISEN 2025-2026/projet BigDAta_IA_Web/travail/IRVE.csv")

# Chargement des packages nécessaires
# dplyr : utilisé pour manipuler les données avec select(), mutate(), filter()
# tidyr : utilisé notamment pour gérer les valeurs manquantes
library(dplyr)
library(tidyr)

# Importation du fichier CSV
# stringsAsFactors = FALSE évite que les colonnes texte soient automatiquement transformées en facteurs
df <- read.csv("E:/ISEN 2025-2026/projet BigDAta_IA_Web/travail/IRVE.csv")

# ------------------------------------------------------------
# 1. Suppression des colonnes inutiles ou trop incomplètes
# ------------------------------------------------------------
# Certaines colonnes ne sont pas utiles pour notre analyse principale
# ou contiennent trop de valeurs manquantes.
# On les retire pour alléger le dataset et faciliter l’analyse.
df_clean$observations <- NULL
df_clean$num_pdl <- NULL
df_clean$raccordement <- NULL
df_clean$cable_t2_attache <- NULL

# ------------------------------------------------------------
# 2. Traitement des valeurs manquantes dans les variables texte
# ------------------------------------------------------------
# Pour les variables catégorielles ou descriptives, on ne supprime pas les lignes.
# On remplace les valeurs manquantes ou vides par "Inconnu" ou "Non renseigne".
# Cela permet de garder les lignes utiles sans confondre valeur manquante et valeur négative.
df_clean <- df_clean %>%
  mutate(
    nom_amenageur = ifelse(
      is.na(nom_amenageur) | trimws(nom_amenageur) == "",
      "Inconnu",
      nom_amenageur
    ),
    
    nom_operateur = ifelse(
      is.na(nom_operateur) | trimws(nom_operateur) == "",
      "Inconnu",
      nom_operateur
    ),
    
    adresse_station = ifelse(
      is.na(adresse_station) | trimws(adresse_station) == "",
      "Non renseigne",
      adresse_station
    ),
    
    code_insee_commune = ifelse(
      is.na(code_insee_commune) | trimws(code_insee_commune) == "",
      "Non renseigne",
      code_insee_commune
    ),
    
    consolidated_commune = ifelse(
      is.na(consolidated_commune) | trimws(consolidated_commune) == "",
      "Non renseigne",
      consolidated_commune
    ),
    
    restriction_gabarit = ifelse(
      is.na(restriction_gabarit) | trimws(restriction_gabarit) == "",
      "Non renseigne",
      restriction_gabarit
    )
  )

# ------------------------------------------------------------
# 3. Harmonisation des variables de paiement et de gratuité
# ------------------------------------------------------------
# Dans le fichier, les valeurs booléennes peuvent être écrites sous plusieurs formes :
# True, true, TRUE, 1 / False, false, FALSE, 0
# On les transforme en trois catégories claires :
# "Oui", "Non", "Non renseigne"
df_clean <- df_clean %>%
  mutate(
    gratuit = case_when(
      gratuit %in% c("True", "true", "TRUE", "1") ~ "Oui",
      gratuit %in% c("False", "false", "FALSE", "0") ~ "Non",
      is.na(gratuit) | trimws(gratuit) == "" ~ "Non renseigne",
      TRUE ~ gratuit
    ),
    
    paiement_cb = case_when(
      paiement_cb %in% c("True", "true", "TRUE", "1") ~ "Oui",
      paiement_cb %in% c("False", "false", "FALSE", "0") ~ "Non",
      is.na(paiement_cb) | trimws(paiement_cb) == "" ~ "Non renseigne",
      TRUE ~ paiement_cb
    ),
    
    paiement_autre = case_when(
      paiement_autre %in% c("True", "true", "TRUE", "1") ~ "Oui",
      paiement_autre %in% c("False", "false", "FALSE", "0") ~ "Non",
      is.na(paiement_autre) | trimws(paiement_autre) == "" ~ "Non renseigne",
      TRUE ~ paiement_autre
    )
  )

# ------------------------------------------------------------
# 4. Création d’un subset pour l’analyse temporelle
# ------------------------------------------------------------
# Pour étudier l’évolution des stations dans le temps,
# il faut obligatoirement une date de mise en service.
# On crée donc un dataset spécifique contenant uniquement les lignes avec une date disponible.
# On ne supprime pas ces lignes du dataset principal, car elles peuvent servir à d’autres analyses.
df_temporel <- df_clean %>%
  filter(
    !is.na(date_mise_en_service),
    trimws(date_mise_en_service) != ""
  )

# ------------------------------------------------------------
# 5. Création d’un subset pour la carte
# ------------------------------------------------------------
# Pour la cartographie, on utilise les coordonnées GPS.
# On garde seulement les lignes avec longitude et latitude disponibles.
# On ajoute aussi un filtre géographique pour garder des coordonnées cohérentes avec la France.
df_carte <- df_clean %>%
  filter(
    !is.na(consolidated_longitude),
    !is.na(consolidated_latitude),
    consolidated_longitude >= -5,
    consolidated_longitude <= 10,
    consolidated_latitude >= 41,
    consolidated_latitude <= 52
  )

# ------------------------------------------------------------
# 6. Création d’un subset pour l’analyse de la tarification
# ------------------------------------------------------------
# La colonne tarification contient beaucoup de valeurs manquantes ou vides.
# On ne l’utilise donc que dans un subset spécifique.
# Cela permet de faire une analyse sur les tarifs sans supprimer trop de lignes du dataset principal.
df_tarif <- df_clean %>%
  filter(
    !is.na(tarification),
    trimws(tarification) != ""
  )

# ------------------------------------------------------------
# 7. Vérification finale des valeurs manquantes ou vides
# ------------------------------------------------------------
# Cette commande compte, pour chaque colonne, le nombre de valeurs NA ou vides.
# Elle permet de vérifier la qualité du dataset après nettoyage.
colSums(is.na(df_clean) | sapply(df_clean, function(x) trimws(as.character(x)) == ""))

dim(df_clean)
dim(df_temporel)
dim(df_tarif)
