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
df_pu <- df_clean[!is.na(df_clean$puissance_nominale) & trimws(df_clean$puissance_nominale) != "", ]

# Vérifications des dimensions
colSums(is.na(df_clean))
dim(df_temporel)
dim(df)
dim(df_regression_tarifs)
dim(df_pu) # Corrigé ici (df_puissance -> df_pu pour éviter une erreur)


# ==============================================================================
# GRAPHIQUE 1 : ÉVOLUTION TEMPORELLE DES INSTALLATIONS
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