# ==============================================================================
# 1. IMPORTATION ET NETTOYAGE DES DONNÉES
# ==============================================================================

df <- read.csv("E:/ISEN 2025-2026/projet BigDAta_IA_Web/travail/IRVE.csv")
df_clean <- df

# Suppression des colonnes inutiles
df_clean$observations <- NULL 
df_clean$num_pdl <- NULL 
df_clean$raccordement <- NULL 
df_clean$cable_t2_attache <- NULL 

# Traitement des valeurs manquantes (NA)
df_clean$nom_operateur[is.na(df_clean$nom_operateur)] <- "Inconnu"
df_clean$gratuit[is.na(df_clean$gratuit)] <- "Non renseigne"
df_clean$paiement_cb[is.na(df_clean$paiement_cb)] <- "Non renseigne"
df_clean$paiement_autre[is.na(df_clean$paiement_autre)] <- "Non renseigne"


# ==============================================================================
# 2. CRÉATION DES SUBSETS
# ==============================================================================

# Subset Évolution Temporelle
df_temporel <- df_clean[!is.na(df_clean$date_mise_en_service) & trimws(df_clean$date_mise_en_service) != "", ]

# Subset Régression Logistique
df_regression_tarifs <- df_clean[!is.na(df_clean$tarification) & trimws(df_clean$tarification) != "", ]

# Subset Puissance
df_pu <- df_clean[!is.na(df_clean$puissance_nominale) & trimws(df_clean$puissance_nominale) != "" & df_clean$puissance_nominale <= 400, ]

# Vérifications des dimensions
colSums(is.na(df_clean))
dim(df_temporel)
dim(df)
dim(df_regression_tarifs)
dim(df_pu) # Corrigé ici (df_puissance -> df_pu pour éviter une erreur)

# GRAPHIQUE 1 : ÉVOLUTION TEMPORELLE DES INSTALLATIONS (ANNUEL)
# ==============================================================================

try(dev.off(), silent = TRUE)

# Extraction de l'année et filtrage (2010 - 2026)
df_temporel$annee_service <- substr(as.character(df_temporel$date_mise_en_service), 1, 4)
table_annees <- table(df_temporel$annee_service)
table_annees_propres <- table_annees[names(table_annees) >= "2010" & names(table_annees) <= "2026"]

# Génération du graphique linéaire
png("graphique_evolution_temps.png", width = 800, height = 600)

plot(as.numeric(names(table_annees_propres)), as.numeric(table_annees_propres),
     type = "b",
     pch = 19,
     lwd = 2,
     col = "darkblue",
     main = "Accélération du déploiement des bornes de recharge par année",
     xlab = "Année de mise en service",
     ylab = "Nombre de raccordements",
     xaxt = "n")

axis(1, at = names(table_annees_propres))
grid()

dev.off()

# ==============================================================================
# ÉVOLUTION TEMPORELLE PLUS DÉTAILLÉE : PAR MOIS
# ==============================================================================

# 1. Convertir la date en vrai format Date
df_temporel$date_service <- as.Date(df_temporel$date_mise_en_service)

# 2. Garder uniquement les dates valides entre 2010 et 2026
df_temporel_mois <- df_temporel[
  !is.na(df_temporel$date_service) &
    df_temporel$date_service >= as.Date("2010-01-01") &
    df_temporel$date_service <= as.Date("2026-12-31"),
]

# 3. Créer une variable année-mois
df_temporel_mois$annee_mois <- format(df_temporel_mois$date_service, "%Y-%m")

# 4. Compter le nombre de points de charge mis en service par mois
table_mois <- table(df_temporel_mois$annee_mois)

# 5. Transformer en dataframe
evol_mois <- data.frame(
  annee_mois = names(table_mois),
  nb_points_charge = as.numeric(table_mois)
)

# 6. Convertir année-mois en date
evol_mois$date <- as.Date(paste0(evol_mois$annee_mois, "-01"))

# 7. Ajouter les mois manquants avec 0
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

# 9. Sauvegarde du graphique
png("evolution_mensuelle_stations.png", width = 1000, height = 600)

plot(evol_complet$date,
     evol_complet$nb_points_charge,
     type = "l",
     col = "red",
     lwd = 1,
     main = "Évolution mensuelle des points de charge mis en service",
     xlab = "Date",
     ylab = "Nombre de points de charge",
     xaxt = "n")

# Axe X lisible : une étiquette tous les 12 mois
dates_axe <- seq.Date(
  from = as.Date("2010-01-01"),
  to = max(evol_complet$date),
  by = "12 months"
)

axis(1,
     at = dates_axe,
     labels = format(dates_axe, "%Y"),
     las = 2)

grid()

dev.off()


# ==============================================================================
# GRAPHIQUE 2 : RÉGRESSION LOGISTIQUE (TARIFS VS PUISSANCE)
# ==============================================================================

try(dev.off(), silent = TRUE)

df_regression_tarifs <- df_regression_tarifs[!is.na(df_regression_tarifs$puissance_nominale), ]

# Classification binaire de la tarification (1 = Payant, 0 = Gratuit)
df_regression_tarifs$tarif_binaire <- ifelse(grepl("€|kwh|kw|/|min", df_regression_tarifs$tarification, ignore.case = TRUE), 1, 0)
df_regression_tarifs$tarif_binaire <- ifelse(grepl("Gratuit|0 pour|0 POUR", df_regression_tarifs$tarification, ignore.case = TRUE), 0, df_regression_tarifs$tarif_binaire)

# Ajustement du modèle logistique
modele_logistique <- glm(tarif_binaire ~ puissance_nominale, 
                         data = df_regression_tarifs, 
                         family = binomial)

# Génération du graphique de régression
png("regression_logistique_tarifs.png", width = 800, height = 600)

plot(df_regression_tarifs$puissance_nominale, df_regression_tarifs$tarif_binaire,
     main = "Probabilité qu'une borne soit payante selon sa puissance",
     xlab = "Puissance nominale de la borne (kW)",
     ylab = "Statut (0 = Gratuit, 1 = Payant)",
     col = "#00000020", 
     pch = 16)

# Calcul et tracé de la courbe de tendance (S-curve)
sequence_puissance <- seq(min(df_regression_tarifs$puissance_nominale), 
                          max(df_regression_tarifs$puissance_nominale), 
                          length.out = 200)

predictions_probabilites <- predict(modele_logistique, 
                                    newdata = data.frame(puissance_nominale = sequence_puissance), 
                                    type = "response")

lines(sequence_puissance, predictions_probabilites, col = "red", lwd = 3)

dev.off()

# ==============================================================================
# VÉRIFICATION DE L'ACCURACY DU MODÈLE LOGISTIQUE
# ==============================================================================

# 1. Prédictions du modèle (seuil 0.5 : >= 0.5 = Payant, < 0.5 = Gratuit)
predictions_classe <- ifelse(
  predict(modele_logistique, type = "response") >= 0.5, 1, 0
)

# 2. Matrice de confusion
matrice_confusion <- table(
  Predicted = predictions_classe, 
  Actual = df_regression_tarifs$tarif_binaire
)
print(matrice_confusion)

# 3. Accuracy globale
accuracy <- sum(diag(matrice_confusion)) / sum(matrice_confusion)
cat("Accuracy :", round(accuracy * 100, 2), "%\n")

# 4. Précision et Rappel
# Tester différents seuils
for (seuil in c(0.50, 0.55, 0.60, 0.65, 0.70)) {
  predictions_classe <- ifelse(
    predict(modele_logistique, type = "response") >= seuil, 1, 0
  )
  mat <- table(Predicted = predictions_classe, 
               Actual = df_regression_tarifs$tarif_binaire)
  acc <- sum(diag(mat)) / sum(mat)
  cat("Seuil:", seuil, "| Accuracy:", round(acc * 100, 2), "%\n")
  print(mat)
  cat("---\n")
}

# ==============================================================================
# 3. CARTOGRAPHIE INTERACTIVE (LEAFLET)
# ==============================================================================

library(leaflet)

# Filtrage des coordonnées valides et limitation à la France métropolitaine
df_carte <- df_clean[!is.na(df_clean$consolidated_longitude) & !is.na(df_clean$consolidated_latitude), ]
df_carte <- df_carte[df_carte$consolidated_longitude != 0 & df_carte$consolidated_latitude != 0, ]
df_carte <- df_carte[df_carte$consolidated_longitude >= -5 & df_carte$consolidated_longitude <= 10, ]
df_carte <- df_carte[df_carte$consolidated_latitude >= 41 & df_carte$consolidated_latitude <= 51, ]

# Génération de la carte avec clusters et popups
carte_clusters <- leaflet(df_carte) %>%
  addTiles() %>%  
  setView(lng = 2.2137, lat = 46.2276, zoom = 5) %>%
  addCircleMarkers(
    lng = ~consolidated_longitude, 
    lat = ~consolidated_latitude,
    radius = 5,
    color = "#0072B2",
    stroke = FALSE, 
    fillOpacity = 0.6,
    clusterOptions = markerClusterOptions(),
    popup = ~paste0(
      "<strong>Borne de recharge</strong><br><br>",
      "<b>Opérateur :</b> ", nom_operateur, "<br>",
      "<b>Puissance :</b> ", puissance_nominale, " kW<br>",
      "<b>Tarification :</b> ", tarification
    )
  )

# Affichage de la carte
carte_clusters

# ==============================================================================
# CARTOGRAPHIE : HEATMAP DES BORNES IRVE
# ==============================================================================

# La heatmap permet de visualiser les zones où les bornes sont les plus concentrées.
# Plus la couleur est intense, plus il y a de points de charge dans la zone.
library(leaflet.extras)
carte_heatmap <- leaflet(df_carte) %>%
  addTiles() %>%
  setView(lng = 2.2137, lat = 46.2276, zoom = 5) %>%
  addHeatmap(
    lng = ~consolidated_longitude,
    lat = ~consolidated_latitude,
    intensity = ~nbre_pdc,
    blur = 20,
    radius = 15,
    max = 0.05
  )

# Affichage de la carte heatmap
carte_heatmap

# ==============================================================================
# HISTOGRAMMES DESCRIPTIFS
# ==============================================================================

# ------------------------------------------------------------
# Histogramme de la puissance nominale
# ------------------------------------------------------------
# On utilise df_pu car il filtre les puissances extrêmes supérieures à 400 kW.
# Cela permet d'avoir un graphique lisible.

png("histogramme_puissance_nominale.png", width = 800, height = 600)

hist(df_pu$puissance_nominale,
     main = "Répartition de la puissance nominale",
     xlab = "Puissance nominale (kW)",
     ylab = "Nombre de points de charge",
     breaks = 50,
     col = "lightblue",
     border = "white")

dev.off()


# ------------------------------------------------------------
# Histogramme du nombre de points de charge
# ------------------------------------------------------------
# On filtre les valeurs très élevées pour éviter que le graphique soit écrasé.

df_pdc <- df_clean[
  !is.na(df_clean$nbre_pdc) &
    df_clean$nbre_pdc > 0 &
    df_clean$nbre_pdc <= 50,
]

png("histogramme_nombre_points_charge.png", width = 800, height = 600)

hist(df_pdc$nbre_pdc,
     main = "Répartition du nombre de points de charge",
     xlab = "Nombre de points de charge",
     ylab = "Nombre de stations",
     breaks = 50,
     col = "lightgreen",
     border = "white")

dev.off()

# ==============================================================================
# CORRÉLATION : PUISSANCE NOMINALE VS NOMBRE DE POINTS DE CHARGE
# ==============================================================================

# Création d'un dataset propre pour la corrélation
# On garde des valeurs réalistes pour éviter que les extrêmes faussent l'analyse.
df_correlation <- df_clean[
  !is.na(df_clean$puissance_nominale) &
    !is.na(df_clean$nbre_pdc) &
    df_clean$puissance_nominale > 0 &
    df_clean$puissance_nominale <= 400 &
    df_clean$nbre_pdc > 0 &
    df_clean$nbre_pdc <= 50,
]

png("correlation_smooth_puissance_pdc.png", width = 800, height = 600)

smoothScatter(df_correlation$nbre_pdc,
              df_correlation$puissance_nominale,
              main = "Relation entre nombre de points de charge et puissance nominale",
              xlab = "Nombre de points de charge",
              ylab = "Puissance nominale (kW)")

modele_lineaire <- lm(puissance_nominale ~ nbre_pdc,
                      data = df_correlation)

abline(modele_lineaire,
       col = "red",
       lwd = 2)

dev.off()

# ==============================================================================
# RÉGRESSION LINÉAIRE
# Objectif : prédire la puissance nominale à partir du nombre de points de charge
# ==============================================================================

modele_lineaire <- lm(puissance_nominale ~ nbre_pdc,
                      data = df_correlation)

# Résumé du modèle
summary(modele_lineaire)

# Graphique de la régression linéaire
png("regression_lineaire_puissance_nbre_pdc.png", width = 800, height = 600)

plot(df_correlation$nbre_pdc,
     df_correlation$puissance_nominale,
     main = "Régression linéaire : puissance nominale selon le nombre de points de charge",
     xlab = "Nombre de points de charge",
     ylab = "Puissance nominale (kW)",
     pch = 16,
     col = rgb(0, 0, 0, 0.2))

abline(modele_lineaire,
       col = "red",
       lwd = 2)

dev.off()

# ==============================================================================
# RÉGRESSION LOGISTIQUE
# Objectif : prédire si une borne est payante ou gratuite selon sa puissance
# ==============================================================================

# On garde seulement les lignes avec tarification et puissance disponibles
# Créer des catégories de puissance
df_logistique <- df_regression_tarifs[
  !is.na(df_regression_tarifs$puissance_nominale) &
    df_regression_tarifs$puissance_nominale > 0 &
    df_regression_tarifs$puissance_nominale <= 400,
]

df_logistique$categorie_puissance <- cut(
  df_logistique$puissance_nominale,
  breaks = c(0, 22, 50, 150, 400),
  labels = c("≤ 22 kW", "22-50 kW", "50-150 kW", "150-400 kW"),
  include.lowest = TRUE
)

# Calculer le taux de bornes payantes par catégorie
taux_payant <- aggregate(
  tarif_binaire ~ categorie_puissance,
  data = df_logistique,
  FUN = mean
)

# Afficher le tableau
taux_payant

# Graphique
png("taux_payant_par_categorie_puissance.png", width = 800, height = 600)

barplot(taux_payant$tarif_binaire,
        names.arg = taux_payant$categorie_puissance,
        main = "Taux de bornes payantes selon la catégorie de puissance",
        xlab = "Catégorie de puissance",
        ylab = "Proportion de bornes payantes",
        ylim = c(0, 1))

dev.off()

