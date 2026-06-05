# ==============================================================================
# PROJET BIG DATA IRVE
# Importation, nettoyage, statistiques descriptives,
# visualisations, cartographie, corrélations et régressions
# ==============================================================================

# ==============================================================================
# 0. PACKAGES
# ==============================================================================

library(dplyr)
library(leaflet)
library(leaflet.extras)


# ==============================================================================
# 1. IMPORTATION DES DONNÉES
# ==============================================================================

df <- read.csv(
  "E:/ISEN 2025-2026/projet BigDAta_IA_Web/travail/IRVE.csv",
  stringsAsFactors = FALSE,
  na.strings = c("", "NA", "N/A")
)

# Vérification des dimensions du dataset brut
dim(df)
nrow(df)
ncol(df)


# ==============================================================================
# 2. FONCTIONS UTILES
# ==============================================================================

# Fonction pour harmoniser les variables booléennes
# Objectif : transformer TRUE / true / 1 / False / 0 en Oui / Non / Non renseigne
harmoniser_bool <- function(x) {
  x <- trimws(tolower(as.character(x)))
  
  ifelse(x %in% c("true", "1", "oui", "yes"), "Oui",
         ifelse(x %in% c("false", "0", "non", "no"), "Non",
                "Non renseigne"))
}


# Fonction pour normaliser les textes de tarification
normaliser_texte <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- tolower(x)
  x <- trimws(x)
  x <- gsub("\\s+", " ", x)
  return(x)
}


# Fonction pour classer la tarification
# Résultat : Payant / Gratuit / Non renseigne / A verifier
classer_tarification <- function(tarification) {
  
  txt <- normaliser_texte(tarification)
  classe <- rep("A verifier", length(txt))
  
  non_renseigne <- txt == "" |
    txt %in% c("na", "n/a", "null", "none", "-", "--", "/", 
               "inconnu", "non renseigne", "non renseignee",
               "non communique", "non communiquee",
               "non precise", "non precisee")
  
  gratuit_mots <- grepl(
    "gratuit|gratuite|gratuits|gratuites|sans frais|offert|offerte|free",
    txt,
    ignore.case = TRUE
  )
  
  gratuit_zero <- grepl(
    "\\b0([,.]0+)?\\s*(€|eur|euro|euros)\\b|\\b0\\s*pour\\b",
    txt,
    ignore.case = TRUE,
    perl = TRUE
  )
  
  prix_non_nul <- grepl(
    "(([1-9][0-9]*([,.][0-9]+)?)|(0[,.]0*[1-9][0-9]*))\\s*(€|eur|euro|euros|cts|ct|centime|centimes)",
    txt,
    ignore.case = TRUE,
    perl = TRUE
  )
  
  payant_mots <- grepl(
    "payant|payante|prix|cout|coût|frais|abonnement|forfait|ttc|ht|roaming|occupation hors charge|par kwh|/kwh|kwh|par minute|/min|par heure|session",
    txt,
    ignore.case = TRUE
  )
  
  classe[(gratuit_mots | gratuit_zero) & !prix_non_nul] <- "Gratuit"
  classe[prix_non_nul | payant_mots] <- "Payant"
  classe[non_renseigne] <- "Non renseigne"
  
  return(classe)
}


# ==============================================================================
# 3. VÉRIFICATION GLOBALE DES COLONNES
# ==============================================================================

verif_colonnes <- data.frame(
  variable = names(df),
  type = sapply(df, class),
  nb_NA = sapply(df, function(x) sum(is.na(x))),
  nb_uniques = sapply(df, function(x) length(unique(x))),
  pct_NA = round(sapply(df, function(x) sum(is.na(x))) / nrow(df) * 100, 2),
  stringsAsFactors = FALSE
)

verif_colonnes <- verif_colonnes[order(-verif_colonnes$pct_NA), ]
verif_colonnes


# ==============================================================================
# 4. VÉRIFICATION DES DOUBLONS
# ==============================================================================

# 4.1 Doublons exacts
nb_doublons_exact <- sum(duplicated(df))
nb_doublons_exact

df_sans_doublons <- df[!duplicated(df), ]

dim(df)
dim(df_sans_doublons)


# 4.2 Vérification spécifique de id_pdc_itinerance
df_sans_doublons$id_pdc_itinerance_clean <- trimws(as.character(df_sans_doublons$id_pdc_itinerance))

df_sans_doublons$id_pdc_itinerance_clean[
  df_sans_doublons$id_pdc_itinerance_clean %in% c("", "Non concerné", "Non concerne", "non concerné", "non concerne")
] <- NA

resume_doublons_pdc <- df_sans_doublons %>%
  filter(!is.na(id_pdc_itinerance_clean)) %>%
  group_by(id_pdc_itinerance_clean) %>%
  summarise(
    nb_lignes = n(),
    nb_stations = n_distinct(id_station_itinerance),
    nb_noms_station = n_distinct(nom_station),
    nb_adresses = n_distinct(adresse_station),
    nb_coordonnees = n_distinct(coordonneesXY),
    nb_puissances = n_distinct(puissance_nominale),
    nb_dates_maj = n_distinct(date_maj),
    nb_resources = n_distinct(datagouv_resource_id),
    .groups = "drop"
  ) %>%
  filter(nb_lignes > 1) %>%
  arrange(desc(nb_lignes))

head(resume_doublons_pdc, 20)


# Séparer les doublons cohérents et incohérents
doublons_coherents <- resume_doublons_pdc %>%
  filter(
    nb_stations == 1,
    nb_adresses == 1,
    nb_coordonnees == 1,
    nb_puissances == 1
  )

doublons_incoherents <- resume_doublons_pdc %>%
  filter(
    nb_stations > 1 |
      nb_adresses > 1 |
      nb_coordonnees > 1 |
      nb_puissances > 1
  )

dim(doublons_coherents)
dim(doublons_incoherents)


# ==============================================================================
# 5. NETTOYAGE DU DATASET
# ==============================================================================

df_clean <- df_sans_doublons

# Suppression des colonnes inutiles ou trop incomplètes
df_clean$observations <- NULL
df_clean$num_pdl <- NULL
df_clean$raccordement <- NULL
df_clean$cable_t2_attache <- NULL

# Remplacement des valeurs manquantes pour certaines variables catégorielles
df_clean$nom_operateur[is.na(df_clean$nom_operateur)] <- "Inconnu"
df_clean$nom_amenageur[is.na(df_clean$nom_amenageur)] <- "Inconnu"

# Harmonisation des variables Oui / Non / Non renseigné
df_clean$gratuit_clean <- harmoniser_bool(df_clean$gratuit)
df_clean$paiement_cb_clean <- harmoniser_bool(df_clean$paiement_cb)
df_clean$paiement_autre_clean <- harmoniser_bool(df_clean$paiement_autre)
df_clean$reservation_clean <- harmoniser_bool(df_clean$reservation)

# Classification de la tarification
df_clean$tarif_classe <- classer_tarification(df_clean$tarification)

# Variable binaire pour la régression logistique
# 1 = Payant
# 0 = Gratuit
# NA = non exploitable pour le modèle
df_clean$tarif_binaire <- ifelse(
  df_clean$tarif_classe == "Payant", 1,
  ifelse(df_clean$tarif_classe == "Gratuit", 0, NA)
)

# Vérification de la classification
table(df_clean$tarif_classe)
round(prop.table(table(df_clean$tarif_classe)) * 100, 2)
table(df_clean$tarif_binaire, useNA = "ifany")


# Conversion de certaines colonnes en numérique
df_clean$puissance_nominale <- as.numeric(df_clean$puissance_nominale)
df_clean$nbre_pdc <- as.numeric(df_clean$nbre_pdc)
df_clean$consolidated_longitude <- as.numeric(df_clean$consolidated_longitude)
df_clean$consolidated_latitude <- as.numeric(df_clean$consolidated_latitude)


# ==============================================================================
# 6. VALEURS ABERRANTES
# ==============================================================================

# 6.1 Valeurs aberrantes sur la puissance nominale
aberrants_puissance <- df_clean[
  !is.na(df_clean$puissance_nominale) &
    (df_clean$puissance_nominale <= 0 | df_clean$puissance_nominale > 400),
]

dim(aberrants_puissance)
summary(aberrants_puissance$puissance_nominale)


# 6.2 Valeurs aberrantes sur le nombre de points de charge
aberrants_pdc <- df_clean[
  !is.na(df_clean$nbre_pdc) &
    (df_clean$nbre_pdc <= 0 | df_clean$nbre_pdc > 50),
]

dim(aberrants_pdc)
summary(aberrants_pdc$nbre_pdc)


# 6.3 Valeurs aberrantes sur les coordonnées GPS
aberrants_coordonnees <- df_clean[
  !is.na(df_clean$consolidated_longitude) &
    !is.na(df_clean$consolidated_latitude) &
    (
      df_clean$consolidated_longitude < -5 |
        df_clean$consolidated_longitude > 10 |
        df_clean$consolidated_latitude < 41 |
        df_clean$consolidated_latitude > 51
    ),
]

dim(aberrants_coordonnees)


# Dataset spécifique pour statistiques et graphiques
df_stats <- df_clean[
  !is.na(df_clean$puissance_nominale) &
    df_clean$puissance_nominale > 0 &
    df_clean$puissance_nominale <= 400 &
    !is.na(df_clean$nbre_pdc) &
    df_clean$nbre_pdc > 0 &
    df_clean$nbre_pdc <= 50,
]

dim(df_clean)
dim(df_stats)


# ==============================================================================
# 7. CRÉATION DES SUBSETS
# ==============================================================================

# Subset temporel
df_temporel <- df_clean[
  !is.na(df_clean$date_mise_en_service),
]

# Subset carte
# ==============================================================================
# FILTRAGE CARTE STRICT : garder seulement les points réellement en France
# ==============================================================================

library(maps)

# 1. Dataset carte de base
df_carte <- df_clean[
  !is.na(df_clean$consolidated_longitude) &
    !is.na(df_clean$consolidated_latitude),
]

# 2. Conversion en numérique
df_carte$consolidated_longitude <- as.numeric(df_carte$consolidated_longitude)
df_carte$consolidated_latitude <- as.numeric(df_carte$consolidated_latitude)

# 3. Enlever les coordonnées impossibles / nulles
df_carte <- df_carte[
  !is.na(df_carte$consolidated_longitude) &
    !is.na(df_carte$consolidated_latitude) &
    df_carte$consolidated_longitude != 0 &
    df_carte$consolidated_latitude != 0,
]

# 4. Premier filtre large autour de la France métropolitaine
df_carte <- df_carte[
  df_carte$consolidated_longitude >= -5.5 &
    df_carte$consolidated_longitude <= 10 &
    df_carte$consolidated_latitude >= 41 &
    df_carte$consolidated_latitude <= 51.5,
]

# 5. Identifier le pays réel selon les coordonnées GPS
df_carte$pays_detecte <- maps::map.where(
  database = "world",
  x = df_carte$consolidated_longitude,
  y = df_carte$consolidated_latitude
)

# 6. Vérifier les pays détectés
table(df_carte$pays_detecte, useNA = "ifany")

# 7. Garder uniquement les points détectés comme France
df_carte <- df_carte[
  !is.na(df_carte$pays_detecte) &
    grepl("^France", df_carte$pays_detecte),
]

# Subset puissance
df_pu <- df_clean[
  !is.na(df_clean$puissance_nominale) &
    df_clean$puissance_nominale > 0 &
    df_clean$puissance_nominale <= 400,
]

# Subset régression logistique
df_logistique <- df_clean[
  !is.na(df_clean$tarif_binaire) &
    !is.na(df_clean$puissance_nominale) &
    df_clean$puissance_nominale > 0 &
    df_clean$puissance_nominale <= 400,
]

dim(df_temporel)
dim(df_carte)
dim(df_pu)
dim(df_logistique)


# ==============================================================================
# 8. STATISTIQUES DESCRIPTIVES
# ==============================================================================

# 8.1 Puissance nominale
summary(df_stats$puissance_nominale)
mean(df_stats$puissance_nominale, na.rm = TRUE)
median(df_stats$puissance_nominale, na.rm = TRUE)
sd(df_stats$puissance_nominale, na.rm = TRUE)
quantile(df_stats$puissance_nominale,
         probs = c(0, 0.25, 0.5, 0.75, 0.90, 0.95, 0.99, 1),
         na.rm = TRUE)

# 8.2 Nombre de points de charge
summary(df_stats$nbre_pdc)
mean(df_stats$nbre_pdc, na.rm = TRUE)
median(df_stats$nbre_pdc, na.rm = TRUE)
sd(df_stats$nbre_pdc, na.rm = TRUE)
quantile(df_stats$nbre_pdc,
         probs = c(0, 0.25, 0.5, 0.75, 0.90, 0.95, 0.99, 1),
         na.rm = TRUE)

# 8.3 Variables catégorielles
sort(table(df_clean$nom_operateur), decreasing = TRUE)[1:10]
sort(table(df_clean$nom_amenageur), decreasing = TRUE)[1:10]
table(df_clean$implantation_station)
table(df_clean$gratuit_clean)
table(df_clean$paiement_cb_clean)
table(df_clean$paiement_autre_clean)


# ==============================================================================
# 9. HISTOGRAMMES
# ==============================================================================

png("histogramme_puissance_nominale.png", width = 800, height = 600)

hist(df_pu$puissance_nominale,
     main = "Répartition de la puissance nominale",
     xlab = "Puissance nominale (kW)",
     ylab = "Nombre de points de charge",
     breaks = 50,
     col = "lightblue",
     border = "white")

dev.off()


png("histogramme_nombre_points_charge.png", width = 800, height = 600)

hist(df_stats$nbre_pdc,
     main = "Répartition du nombre de points de charge",
     xlab = "Nombre de points de charge",
     ylab = "Nombre de stations",
     breaks = 50,
     col = "lightgreen",
     border = "white")

dev.off()


# ==============================================================================
# 10. ÉVOLUTION TEMPORELLE
# ==============================================================================

df_temporel$date_service <- as.Date(df_temporel$date_mise_en_service)

df_temporel_mois <- df_temporel[
  !is.na(df_temporel$date_service) &
    df_temporel$date_service >= as.Date("2010-01-01") &
    df_temporel$date_service <= as.Date("2026-12-31"),
]

df_temporel_mois$annee_mois <- format(df_temporel_mois$date_service, "%Y-%m")

table_mois <- table(df_temporel_mois$annee_mois)

evol_mois <- data.frame(
  annee_mois = names(table_mois),
  nb_points_charge = as.numeric(table_mois)
)

evol_mois$date <- as.Date(paste0(evol_mois$annee_mois, "-01"))

toutes_dates <- seq.Date(
  from = min(evol_mois$date),
  to = max(evol_mois$date),
  by = "month"
)

evol_complet <- data.frame(date = toutes_dates)

evol_complet <- merge(
  evol_complet,
  evol_mois[, c("date", "nb_points_charge")],
  by = "date",
  all.x = TRUE
)

evol_complet$nb_points_charge[is.na(evol_complet$nb_points_charge)] <- 0

png("evolution_mensuelle_points_charge.png", width = 1000, height = 600)

plot(evol_complet$date,
     evol_complet$nb_points_charge,
     type = "l",
     col = "darkblue",
     lwd = 1,
     main = "Évolution mensuelle des points de charge mis en service",
     xlab = "Date",
     ylab = "Nombre de points de charge",
     xaxt = "n")

dates_axe <- seq.Date(
  from = as.Date("2010-01-01"),
  to = max(evol_complet$date),
  by = "12 months"
)

axis(1, at = dates_axe, labels = format(dates_axe, "%Y"), las = 2)
grid()

dev.off()


# ==============================================================================
# 11. CARTOGRAPHIE ENRICHIE : CLUSTERS + COULEUR SELON PUISSANCE
# ==============================================================================

# On garde les points avec une puissance connue et positive
df_carte_puissance <- df_carte[
  !is.na(df_carte$puissance_nominale) &
    df_carte$puissance_nominale > 0,
]

# Création des catégories de puissance
df_carte_puissance$categorie_puissance <- cut(
  df_carte_puissance$puissance_nominale,
  breaks = c(0, 22, 50, 150, 400, Inf),
  labels = c(
    "Lente (≤ 22 kW)",
    "Moyenne (22-50 kW)",
    "Rapide (50-150 kW)",
    "Ultra-rapide (150-400 kW)",
    "Extrême (> 400 kW)"
  ),
  include.lowest = TRUE
)

# Palette de couleurs selon la catégorie de puissance
palette_puissance <- colorFactor(
  palette = c("green", "yellow", "orange", "red", "purple"),
  domain = df_carte_puissance$categorie_puissance
)

# Carte enrichie avec clusters et coloration par puissance
carte_clusters <- leaflet(df_carte_puissance) %>%
  addTiles() %>%
  setView(lng = 2.2137, lat = 46.2276, zoom = 5) %>%
  addCircleMarkers(
    lng = ~consolidated_longitude,
    lat = ~consolidated_latitude,
    radius = 5,
    color = ~palette_puissance(categorie_puissance),
    stroke = FALSE,
    fillOpacity = 0.7,
    clusterOptions = markerClusterOptions(),
    popup = ~paste0(
      "<strong>Borne de recharge</strong><br><br>",
      "<b>Station :</b> ", nom_station, "<br>",
      "<b>Opérateur :</b> ", nom_operateur, "<br>",
      "<b>Puissance :</b> ", puissance_nominale, " kW<br>",
      "<b>Catégorie :</b> ", categorie_puissance, "<br>",
      "<b>Nombre de points de charge :</b> ", nbre_pdc, "<br>",
      "<b>Tarification :</b> ", tarif_classe
    )
  ) %>%
  addLegend(
    position = "bottomright",
    pal = palette_puissance,
    values = ~categorie_puissance,
    title = "Puissance nominale",
    opacity = 1
  )

carte_clusters


# ==============================================================================
# 12. CARTOGRAPHIE : HEATMAP
# ==============================================================================

carte_heatmap <- leaflet(df_carte) %>%
  addTiles() %>%
  setView(lng = 2.2137, lat = 46.2276, zoom = 5) %>%
  leaflet.extras::addHeatmap(
    lng = ~consolidated_longitude,
    lat = ~consolidated_latitude,
    intensity = ~nbre_pdc,
    blur = 20,
    radius = 15
  )

carte_heatmap


# ==============================================================================
# 13. CORRÉLATION : PUISSANCE NOMINALE VS NOMBRE DE POINTS DE CHARGE
# ==============================================================================

cor.test(df_stats$puissance_nominale,
         df_stats$nbre_pdc,
         method = "spearman")

modele_lineaire <- lm(puissance_nominale ~ nbre_pdc, data = df_stats)

summary(modele_lineaire)

png("correlation_smooth_puissance_pdc.png", width = 800, height = 600)

smoothScatter(df_stats$nbre_pdc,
              df_stats$puissance_nominale,
              main = "Relation entre nombre de points de charge et puissance nominale",
              xlab = "Nombre de points de charge",
              ylab = "Puissance nominale (kW)")

abline(modele_lineaire, col = "red", lwd = 2)

dev.off()


# ==============================================================================
# 14. BOXPLOT : NOMBRE DE POINTS DE CHARGE PAR CATÉGORIE DE PUISSANCE
# ==============================================================================

df_stats$categorie_puissance <- cut(
  df_stats$puissance_nominale,
  breaks = c(0, 22, 50, 150, 400),
  labels = c("Lente (≤22 kW)", "Moyenne (22-50 kW)", "Rapide (50-150 kW)", "Ultra-rapide (150-400 kW)"),
  include.lowest = TRUE
)

table(df_stats$categorie_puissance)

png("boxplot_nbre_pdc_par_categorie_puissance.png", width = 1000, height = 600)

boxplot(nbre_pdc ~ categorie_puissance,
        data = df_stats,
        main = "Nombre de points de charge selon la catégorie de puissance",
        xlab = "Catégorie de puissance",
        ylab = "Nombre de points de charge",
        col = c("lightblue", "lightgreen", "orange", "tomato"),
        las = 2)

dev.off()

kruskal.test(nbre_pdc ~ categorie_puissance, data = df_stats)


# ==============================================================================
# 15. RELATION ENTRE PUISSANCE ET PAIEMENT CB / GRATUITÉ
# ==============================================================================

png("puissance_selon_paiement_cb.png", width = 800, height = 600)

boxplot(puissance_nominale ~ paiement_cb_clean,
        data = df_pu,
        main = "Puissance nominale selon le paiement par carte bancaire",
        xlab = "Paiement CB",
        ylab = "Puissance nominale (kW)",
        col = c("lightblue", "lightgreen", "orange"))

dev.off()

kruskal.test(puissance_nominale ~ paiement_cb_clean, data = df_pu)


png("puissance_selon_gratuite.png", width = 800, height = 600)

boxplot(puissance_nominale ~ gratuit_clean,
        data = df_pu,
        main = "Puissance nominale selon la gratuité",
        xlab = "Gratuit",
        ylab = "Puissance nominale (kW)",
        col = c("lightblue", "lightgreen", "orange"))

dev.off()

kruskal.test(puissance_nominale ~ gratuit_clean, data = df_pu)

# ==============================================================================
# 16. PARTS DE MARCHÉ DES OPÉRATEURS - VERSION HORIZONTALE
# ==============================================================================

# 1. Compter le nombre de points de charge par opérateur
top_operateurs <- sort(table(df_clean$nom_operateur), decreasing = TRUE)

# 2. Garder les 10 premiers opérateurs
top10_operateurs <- top_operateurs[1:10]

# 3. Regrouper tous les autres dans "Autres"
autres_operateurs <- sum(top_operateurs[-(1:10)])

parts_operateurs <- c(top10_operateurs, Autres = autres_operateurs)

# 4. Calculer les pourcentages
parts_operateurs_pct <- round(parts_operateurs / sum(parts_operateurs) * 100, 2)

# 5. Créer un tableau récapitulatif
table_parts_operateurs <- data.frame(
  Operateur = names(parts_operateurs),
  Nombre_points_charge = as.numeric(parts_operateurs),
  Pourcentage = as.numeric(parts_operateurs_pct)
)

table_parts_operateurs

# 6. Sauvegarder le graphique horizontal
png("parts_marche_operateurs_horizontal.png", width = 1000, height = 700)

par(mar = c(5, 12, 4, 2))  # augmente la marge à gauche pour les noms

barplot(
  rev(parts_operateurs_pct),                      # rev pour avoir le plus grand en haut
  horiz = TRUE,
  las = 1,
  col = "lightblue",
  main = "Parts de marché des principaux opérateurs",
  xlab = "Part de marché (%)",
  names.arg = rev(names(parts_operateurs)),
  xlim = c(0, max(parts_operateurs_pct) + 5)
)

# 7. Ajouter les pourcentages au bout des barres
text(
  x = rev(parts_operateurs_pct) + 1,
  y = seq_along(parts_operateurs_pct),
  labels = paste0(rev(parts_operateurs_pct), "%"),
  cex = 0.9,
  adj = 0
)

dev.off()

# ==============================================================================
# 17. PRÉDICTION DE LA PUISSANCE NOMINALE D'UNE STATION
# Régression linéaire multiple au niveau station
# ==============================================================================

library(dplyr)

# ------------------------------------------------------------------------------
# 1. Fonction pour prendre la modalité la plus fréquente
# ------------------------------------------------------------------------------

mode_stat <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA)
  names(sort(table(x), decreasing = TRUE))[1]
}

# ------------------------------------------------------------------------------
# 2. Préparation des variables de prises
# ------------------------------------------------------------------------------

df_clean$prise_type_2_clean <- ifelse(
  tolower(as.character(df_clean$prise_type_2)) %in% c("true", "1", "oui"),
  "Oui",
  "Non"
)

df_clean$combo_ccs_clean <- ifelse(
  tolower(as.character(df_clean$prise_type_combo_ccs)) %in% c("true", "1", "oui"),
  "Oui",
  "Non"
)

df_clean$chademo_clean <- ifelse(
  tolower(as.character(df_clean$prise_type_chademo)) %in% c("true", "1", "oui"),
  "Oui",
  "Non"
)

df_clean$prise_ef_clean <- ifelse(
  tolower(as.character(df_clean$prise_type_ef)) %in% c("true", "1", "oui"),
  "Oui",
  "Non"
)

# ------------------------------------------------------------------------------
# 3. Filtrer les données exploitables
# ------------------------------------------------------------------------------

df_puissance_base <- df_clean %>%
  filter(
    !is.na(id_station_itinerance),
    !is.na(puissance_nominale),
    puissance_nominale > 0,
    puissance_nominale <= 400,
    !is.na(nbre_pdc),
    nbre_pdc > 0,
    nbre_pdc <= 50
  )

# ------------------------------------------------------------------------------
# 4. Agréger au niveau station
# ------------------------------------------------------------------------------

df_station <- df_puissance_base %>%
  group_by(id_station_itinerance) %>%
  summarise(
    # Variable cible : puissance maximale disponible dans la station
    puissance_station = max(puissance_nominale, na.rm = TRUE),
    
    # Variables explicatives
    nbre_pdc_station = max(nbre_pdc, na.rm = TRUE),
    
    has_type2 = ifelse(any(prise_type_2_clean == "Oui"), "Oui", "Non"),
    has_combo_ccs = ifelse(any(combo_ccs_clean == "Oui"), "Oui", "Non"),
    has_chademo = ifelse(any(chademo_clean == "Oui"), "Oui", "Non"),
    has_ef = ifelse(any(prise_ef_clean == "Oui"), "Oui", "Non"),
    
    implantation_station = mode_stat(implantation_station),
    paiement_cb_clean = mode_stat(paiement_cb_clean),
    gratuit_clean = mode_stat(gratuit_clean),
    tarif_classe = mode_stat(tarif_classe),
    
    .groups = "drop"
  )

# Supprimer les lignes incomplètes
df_station <- na.omit(df_station)

# Conversion en facteurs
df_station$has_type2 <- as.factor(df_station$has_type2)
df_station$has_combo_ccs <- as.factor(df_station$has_combo_ccs)
df_station$has_chademo <- as.factor(df_station$has_chademo)
df_station$has_ef <- as.factor(df_station$has_ef)
df_station$implantation_station <- as.factor(df_station$implantation_station)
df_station$paiement_cb_clean <- as.factor(df_station$paiement_cb_clean)
df_station$gratuit_clean <- as.factor(df_station$gratuit_clean)
df_station$tarif_classe <- as.factor(df_station$tarif_classe)

# Vérification
dim(df_station)
summary(df_station$puissance_station)

# ==============================================================================
# MODÈLE DE RÉGRESSION LINÉAIRE MULTIPLE
# ==============================================================================

set.seed(123)

index_train_station <- sample(
  1:nrow(df_station),
  size = 0.8 * nrow(df_station)
)

train_station <- df_station[index_train_station, ]
test_station <- df_station[-index_train_station, ]

# Modèle avec transformation log pour réduire l'effet des grandes puissances
modele_puissance_station <- lm(
  log1p(puissance_station) ~ log1p(nbre_pdc_station) +
    has_type2 +
    has_combo_ccs +
    has_chademo +
    has_ef +
    implantation_station +
    paiement_cb_clean +
    gratuit_clean +
    tarif_classe,
  data = train_station
)

summary(modele_puissance_station)

# ==============================================================================
# 18. ÉVALUATION DU MODÈLE
# ==============================================================================

# Prédiction sur le test set
pred_log <- predict(
  modele_puissance_station,
  newdata = test_station
)

# Retour à l'échelle normale
pred_puissance_station <- expm1(pred_log)

# Empêcher les prédictions négatives
pred_puissance_station[pred_puissance_station < 0] <- 0

# Erreurs
erreurs <- test_station$puissance_station - pred_puissance_station

MAE <- mean(abs(erreurs), na.rm = TRUE)
RMSE <- sqrt(mean(erreurs^2, na.rm = TRUE))

SSE <- sum(erreurs^2, na.rm = TRUE)
SST <- sum(
  (test_station$puissance_station - mean(test_station$puissance_station, na.rm = TRUE))^2,
  na.rm = TRUE
)

R2_test <- 1 - SSE / SST

cat("MAE :", round(MAE, 2), "kW\n")
cat("RMSE :", round(RMSE, 2), "kW\n")
cat("R² test :", round(R2_test, 4), "\n")

# ==============================================================================
# GRAPHIQUE : PUISSANCE RÉELLE VS PUISSANCE PRÉDITE
# ==============================================================================

png("prediction_puissance_station_reelle_vs_predite.png", width = 800, height = 600)

plot(
  test_station$puissance_station,
  pred_puissance_station,
  main = "Prédiction de la puissance nominale d'une station",
  xlab = "Puissance réelle (kW)",
  ylab = "Puissance prédite (kW)",
  pch = 16,
  col = rgb(0, 0, 0, 0.25)
)

abline(0, 1, col = "red", lwd = 3)

grid()

dev.off()

# ==============================================================================
# MOSAICPLOT PROPRE : CATÉGORIE DE PUISSANCE × PAIEMENT CB
# ==============================================================================

# install.packages("vcd") # à faire une seule fois si besoin
library(vcd)
library(grid)

# ------------------------------------------------------------------------------
# 1. Préparer les données
# ------------------------------------------------------------------------------

df_mosaic_puissance <- df_clean[
  !is.na(df_clean$puissance_nominale) &
    df_clean$puissance_nominale > 0 &
    df_clean$puissance_nominale <= 400 &
    df_clean$paiement_cb_clean %in% c("Oui", "Non"),
]

# ------------------------------------------------------------------------------
# 2. Créer une catégorie de puissance simple
# ------------------------------------------------------------------------------

df_mosaic_puissance$categorie_puissance_simple <- cut(
  df_mosaic_puissance$puissance_nominale,
  breaks = c(0, 22, 150, 400),
  labels = c(
    "Faible / normale\n≤ 22 kW",
    "Rapide\n22-150 kW",
    "Très rapide\n150-400 kW"
  ),
  include.lowest = TRUE
)

# ------------------------------------------------------------------------------
# 3. Tableau croisé
# ------------------------------------------------------------------------------

tab_puissance_cb <- table(
  "Catégorie de puissance" = df_mosaic_puissance$categorie_puissance_simple,
  "Paiement CB" = df_mosaic_puissance$paiement_cb_clean
)

tab_puissance_cb

# ------------------------------------------------------------------------------
# 4. Test du Khi-deux + Cramer's V
# ------------------------------------------------------------------------------

test_chi2 <- chisq.test(tab_puissance_cb)

cramer_v <- sqrt(
  as.numeric(test_chi2$statistic) /
    (sum(tab_puissance_cb) * (min(dim(tab_puissance_cb)) - 1))
)

p_value_txt <- ifelse(
  test_chi2$p.value < 0.001,
  "p-value < 0.001",
  paste0("p-value = ", round(test_chi2$p.value, 4))
)

cramer_txt <- paste0("Cramer's V = ", round(cramer_v, 3))

# ------------------------------------------------------------------------------
# 5. Mosaicplot propre
# ------------------------------------------------------------------------------

png(
  "mosaicplot_puissance_paiement_cb.png",
  width = 1100,
  height = 750,
  res = 120
)

mosaic(
  tab_puissance_cb,
  shade = TRUE,
  legend = TRUE,
  main = "Association entre puissance nominale et paiement par carte bancaire",
  labeling_args = list(
    gp_labels = gpar(fontsize = 10),
    gp_varnames = gpar(fontsize = 12, fontface = "bold")
  )
)

grid.text(
  paste(p_value_txt, "|", cramer_txt),
  x = 0.68,
  y = 0.07,
  gp = gpar(fontsize = 11, fontface = "bold")
)

dev.off()

# ==============================================================================
# PRÉDICTION DE LA TARIFICATION EN 3 GROUPES : BAS / MODÉRÉ / ÉLEVÉ
# ==============================================================================

# Package pour la régression logistique multinomiale
# install.packages("nnet") # à faire une seule fois si besoin
library(nnet)

# ------------------------------------------------------------------------------
# 1. Fonction pour normaliser le texte de tarification
# ------------------------------------------------------------------------------

normaliser_texte <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- tolower(x)
  x <- trimws(x)
  x <- gsub("\\s+", " ", x)
  return(x)
}

# ------------------------------------------------------------------------------
# 2. Fonction pour extraire un nombre depuis un texte
# ------------------------------------------------------------------------------

extraire_nombre <- function(pattern, texte) {
  m <- regexec(pattern, texte, perl = TRUE)
  reg <- regmatches(texte, m)
  
  out <- rep(NA_character_, length(texte))
  
  for (i in seq_along(reg)) {
    if (length(reg[[i]]) >= 2) {
      out[i] <- reg[[i]][2]
    }
  }
  
  out <- gsub(",", ".", out)
  return(as.numeric(out))
}

# ------------------------------------------------------------------------------
# 3. Extraire un prix comparable en €/kWh
# ------------------------------------------------------------------------------

extraire_prix_kwh <- function(tarification) {
  
  txt <- normaliser_texte(tarification)
  
  prix <- rep(NA_real_, length(txt))
  
  # Cas gratuits
  est_gratuit <- grepl(
    "gratuit|gratuite|sans frais|offert|free|0\\s*(€|eur|euro|euros)",
    txt,
    ignore.case = TRUE,
    perl = TRUE
  )
  
  # Prix en euros par kWh : ex. 0,29 €/kWh, 0.35 euro/kWh
  prix_euro_kwh <- extraire_nombre(
    "([0-9]+(?:[\\.,][0-9]+)?)\\s*(?:€|eur|euro|euros).*?kwh",
    txt
  )
  
  # Prix en centimes par kWh : ex. 35 cts/kWh
  prix_cts_kwh <- extraire_nombre(
    "([0-9]+(?:[\\.,][0-9]+)?)\\s*(?:cts|ct|centime|centimes).*?kwh",
    txt
  )
  
  # Conversion centimes -> euros
  prix_cts_kwh <- prix_cts_kwh / 100
  
  # Priorité au prix en euros/kWh
  prix[!is.na(prix_euro_kwh)] <- prix_euro_kwh[!is.na(prix_euro_kwh)]
  
  # Si pas de prix euro mais prix en centimes
  prix[is.na(prix) & !is.na(prix_cts_kwh)] <- prix_cts_kwh[is.na(prix) & !is.na(prix_cts_kwh)]
  
  # Cas gratuits sans prix non nul détecté
  prix[is.na(prix) & est_gratuit] <- 0
  
  return(prix)
}

# ------------------------------------------------------------------------------
# 4. Application : création d'un prix numérique en €/kWh
# ------------------------------------------------------------------------------

df_clean$prix_kwh_eur <- extraire_prix_kwh(df_clean$tarification)

# Vérifier combien de lignes ont un prix exploitable
summary(df_clean$prix_kwh_eur)
sum(!is.na(df_clean$prix_kwh_eur))
sum(is.na(df_clean$prix_kwh_eur))

# ------------------------------------------------------------------------------
# 5. Création du dataset pour la prédiction de la tarification
# ------------------------------------------------------------------------------
df_tarif_3 <- df_clean[
  !is.na(df_clean$prix_kwh_eur) &
    df_clean$prix_kwh_eur >= 0 &
    df_clean$prix_kwh_eur <= 2 &
    !is.na(df_clean$puissance_nominale) &
    !is.na(df_clean$nbre_pdc) &
    df_clean$puissance_nominale > 0 &
    df_clean$puissance_nominale <= 400 &
    df_clean$nbre_pdc > 0 &
    df_clean$nbre_pdc <= 50,
]

# ------------------------------------------------------------------------------
# 6. Regrouper la tarification en 3 groupes : bas / modéré / élevé
# ------------------------------------------------------------------------------

# Méthode data-driven : on coupe en trois groupes selon les tertiles
seuils <- quantile(
  df_tarif_3$prix_kwh_eur,
  probs = c(1/3, 2/3),
  na.rm = TRUE
)

seuils

df_tarif_3$tarif_groupe <- cut(
  df_tarif_3$prix_kwh_eur,
  breaks = c(-Inf, seuils[1], seuils[2], Inf),
  labels = c("Bas", "Modere", "Eleve"),
  include.lowest = TRUE
)

# Vérification de la répartition des groupes
table(df_tarif_3$tarif_groupe)
round(prop.table(table(df_tarif_3$tarif_groupe)) * 100, 2)

# ------------------------------------------------------------------------------
# 7. Préparer les variables explicatives
# ------------------------------------------------------------------------------

df_tarif_3$tarif_groupe <- as.factor(df_tarif_3$tarif_groupe)
df_tarif_3$paiement_cb_clean <- as.factor(df_tarif_3$paiement_cb_clean)
df_tarif_3$gratuit_clean <- as.factor(df_tarif_3$gratuit_clean)
df_tarif_3$implantation_station <- as.factor(df_tarif_3$implantation_station)

# Harmoniser prise Combo CCS si ce n'est pas déjà fait
df_tarif_3$combo_ccs_clean <- ifelse(
  tolower(as.character(df_tarif_3$prise_type_combo_ccs)) %in% c("true", "1"),
  "Oui",
  "Non"
)

df_tarif_3$combo_ccs_clean <- as.factor(df_tarif_3$combo_ccs_clean)

# ------------------------------------------------------------------------------
# 8. Séparer train / test
# ------------------------------------------------------------------------------

set.seed(123)

index_train <- sample(
  1:nrow(df_tarif_3),
  size = 0.8 * nrow(df_tarif_3)
)

train <- df_tarif_3[index_train, ]
test <- df_tarif_3[-index_train, ]

# ------------------------------------------------------------------------------
# 9. Modèle de régression logistique multinomiale
# ------------------------------------------------------------------------------

modele_tarif_3 <- multinom(
  tarif_groupe ~ puissance_nominale +
    nbre_pdc +
    paiement_cb_clean +
    gratuit_clean +
    combo_ccs_clean +
    implantation_station,
  data = train,
  trace = FALSE
)

summary(modele_tarif_3)

# ------------------------------------------------------------------------------
# 10. Prédiction sur le test set
# ------------------------------------------------------------------------------

pred_tarif <- predict(
  modele_tarif_3,
  newdata = test
)

# Matrice de confusion
matrice_confusion_tarif <- table(
  Predicted = pred_tarif,
  Actual = test$tarif_groupe
)

matrice_confusion_tarif

# Accuracy
accuracy_tarif <- sum(diag(matrice_confusion_tarif)) / sum(matrice_confusion_tarif)

cat("Accuracy du modèle :", round(accuracy_tarif * 100, 2), "%\n")

# ==============================================================================
# COURBES DE PROBABILITÉ POUR LA RÉGRESSION MULTINOMIALE
# Tarif Bas / Modéré / Élevé selon la puissance nominale
# ==============================================================================

# 1. Créer une séquence de puissance pour tracer les courbes
sequence_puissance <- seq(
  from = min(df_tarif_3$puissance_nominale, na.rm = TRUE),
  to = max(df_tarif_3$puissance_nominale, na.rm = TRUE),
  length.out = 300
)

# 2. Créer un dataset de prédiction
# On fixe les autres variables à une valeur représentative
newdata_tarif <- data.frame(
  puissance_nominale = sequence_puissance,
  nbre_pdc = median(df_tarif_3$nbre_pdc, na.rm = TRUE),
  paiement_cb_clean = names(sort(table(df_tarif_3$paiement_cb_clean), decreasing = TRUE))[1],
  gratuit_clean = names(sort(table(df_tarif_3$gratuit_clean), decreasing = TRUE))[1],
  combo_ccs_clean = names(sort(table(df_tarif_3$combo_ccs_clean), decreasing = TRUE))[1],
  implantation_station = names(sort(table(df_tarif_3$implantation_station), decreasing = TRUE))[1]
)

# 3. S'assurer que les variables catégorielles ont les mêmes niveaux que dans le modèle
newdata_tarif$paiement_cb_clean <- factor(
  newdata_tarif$paiement_cb_clean,
  levels = levels(train$paiement_cb_clean)
)

newdata_tarif$gratuit_clean <- factor(
  newdata_tarif$gratuit_clean,
  levels = levels(train$gratuit_clean)
)

newdata_tarif$combo_ccs_clean <- factor(
  newdata_tarif$combo_ccs_clean,
  levels = levels(train$combo_ccs_clean)
)

newdata_tarif$implantation_station <- factor(
  newdata_tarif$implantation_station,
  levels = levels(train$implantation_station)
)

# 4. Prédire les probabilités pour chaque groupe tarifaire
probas_tarif <- predict(
  modele_tarif_3,
  newdata = newdata_tarif,
  type = "probs"
)

# 5. Transformer en dataframe
probas_tarif_df <- data.frame(
  puissance_nominale = sequence_puissance,
  probas_tarif
)

# 6. Tracer les courbes
png("courbes_tarification_multinomiale.png", width = 900, height = 600)

plot(
  probas_tarif_df$puissance_nominale,
  probas_tarif_df$Bas,
  type = "l",
  lwd = 3,
  col = "green",
  ylim = c(0, 1),
  main = "Probabilité prédite des groupes de tarification selon la puissance",
  xlab = "Puissance nominale (kW)",
  ylab = "Probabilité prédite"
)

lines(
  probas_tarif_df$puissance_nominale,
  probas_tarif_df$Modere,
  lwd = 3,
  col = "orange"
)

lines(
  probas_tarif_df$puissance_nominale,
  probas_tarif_df$Eleve,
  lwd = 3,
  col = "red"
)

grid()

legend(
  "right",
  legend = c("Bas", "Modéré", "Élevé"),
  col = c("green", "orange", "red"),
  lwd = 3,
  bty = "n"
)

dev.off()

# ==============================================================================
# EXPORT DU FICHIER NETTOYÉ POUR LA PARTIE IA
# ==============================================================================

df_export <- df_clean

# Colonne technique utilisée seulement pour vérifier les doublons
df_export$id_pdc_itinerance_clean <- NULL

write.csv(
  df_export,
  "export_IA.csv",
  row.names = FALSE
)

file.exists("export_IA.csv")
dim(df_export)

