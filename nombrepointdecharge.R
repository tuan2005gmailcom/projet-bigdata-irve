# ------------------------------------------------------------
# ANALYSE DÉTAILLÉE DE LA VARIABLE nbre_pdc
# ------------------------------------------------------------

# 1. Vérifier le type de la variable
class(df_clean$nbre_pdc)
str(df_clean$nbre_pdc)

# 2. Résumé statistique général
summary(df_clean$nbre_pdc)

# 3. Statistiques principales
mean(df_clean$nbre_pdc, na.rm = TRUE)      # moyenne
median(df_clean$nbre_pdc, na.rm = TRUE)    # médiane
min(df_clean$nbre_pdc, na.rm = TRUE)       # valeur minimale
max(df_clean$nbre_pdc, na.rm = TRUE)       # valeur maximale
sd(df_clean$nbre_pdc, na.rm = TRUE)        # écart-type

# 4. Quantiles pour voir la distribution
quantile(df_clean$nbre_pdc,
         probs = c(0, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99, 1),
         na.rm = TRUE)

# 5. Compter les valeurs nulles ou négatives
sum(df_clean$nbre_pdc == 0, na.rm = TRUE)
sum(df_clean$nbre_pdc < 0, na.rm = TRUE)

# 6. Voir les valeurs les plus fréquentes
sort(table(df_clean$nbre_pdc), decreasing = TRUE)[1:20]

# 7. Détecter les stations avec un nombre très élevé de points de charge
df_pdc_extreme <- df_clean[df_clean$nbre_pdc > 20, ]

dim(df_pdc_extreme)
summary(df_pdc_extreme$nbre_pdc)

# 8. Créer un subset plus propre pour les graphiques
# Ici on garde les stations avec un nombre de points de charge entre 1 et 20
df_pdc <- df_clean[
  df_clean$nbre_pdc > 0 &
    df_clean$nbre_pdc <= 20,
]

dim(df_pdc)
summary(df_pdc$nbre_pdc)

# 9. Histogramme du nombre de points de charge
hist(df_pdc$nbre_pdc,
     main = "Répartition du nombre de points de charge",
     xlab = "Nombre de points de charge",
     ylab = "Nombre de stations",
     breaks = 20)

# 10. Boxplot pour repérer les valeurs atypiques
boxplot(df_clean$nbre_pdc,
        main = "Boxplot du nombre de points de charge",
        ylab = "Nombre de points de charge")

# 11. Boxplot sur les valeurs filtrées
boxplot(df_pdc$nbre_pdc,
        main = "Boxplot du nombre de points de charge filtré",
        ylab = "Nombre de points de charge")

# 12. Créer des catégories de taille de station
df_clean$categorie_pdc <- cut(
  df_clean$nbre_pdc,
  breaks = c(-Inf, 1, 2, 5, 10, 20, Inf),
  labels = c("1 point", "2 points", "3 à 5 points", "6 à 10 points", "11 à 20 points", "Plus de 20 points")
)

# 13. Compter les catégories
table(df_clean$categorie_pdc)

# 14. Pourcentage par catégorie
prop.table(table(df_clean$categorie_pdc)) * 100

# 15. Graphique des catégories
barplot(table(df_clean$categorie_pdc),
        main = "Répartition des stations selon le nombre de points de charge",
        xlab = "Catégorie",
        ylab = "Nombre de stations",
        las = 2)
