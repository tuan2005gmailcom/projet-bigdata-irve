# ------------------------------------------------------------
# ANALYSE DÉTAILLÉE DE LA VARIABLE puissance_nominale
# ------------------------------------------------------------

# 1. Vérifier le type de la variable
class(df_clean$puissance_nominale)
str(df_clean$puissance_nominale)

# 2. Résumé statistique général
summary(df_clean$puissance_nominale)

# 3. Statistiques principales
mean(df_clean$puissance_nominale, na.rm = TRUE)      # moyenne
median(df_clean$puissance_nominale, na.rm = TRUE)    # médiane
min(df_clean$puissance_nominale, na.rm = TRUE)       # valeur minimale
max(df_clean$puissance_nominale, na.rm = TRUE)       # valeur maximale
sd(df_clean$puissance_nominale, na.rm = TRUE)        # écart-type

# 4. Quantiles pour voir la distribution
quantile(df_clean$puissance_nominale,
         probs = c(0, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99, 1),
         na.rm = TRUE)

# 5. Compter les valeurs nulles ou négatives
sum(df_clean$puissance_nominale == 0, na.rm = TRUE)
sum(df_clean$puissance_nominale < 0, na.rm = TRUE)

# 6. Voir les valeurs les plus fréquentes
sort(table(df_clean$puissance_nominale), decreasing = TRUE)[1:20]

# 7. Détecter les valeurs très élevées
df_puissance_extreme <- df_clean[df_clean$puissance_nominale > 400, ]

dim(df_puissance_extreme)
summary(df_puissance_extreme$puissance_nominale)

# 8. Créer un subset plus propre pour les graphiques et analyses
df_puissance <- df_clean[
  df_clean$puissance_nominale > 0 &
    df_clean$puissance_nominale <= 400,
]

dim(df_puissance)
summary(df_puissance$puissance_nominale)

# 9. Histogramme simple de la puissance nominale
hist(df_puissance$puissance_nominale,
     main = "Répartition de la puissance nominale",
     xlab = "Puissance nominale (kW)",
     ylab = "Nombre de points de charge",
     breaks = 50)

# 10. Boxplot pour repérer les valeurs atypiques
boxplot(df_clean$puissance_nominale,
        main = "Boxplot de la puissance nominale",
        ylab = "Puissance nominale (kW)")

# 11. Boxplot sur les valeurs filtrées
boxplot(df_puissance$puissance_nominale,
        main = "Boxplot de la puissance nominale filtrée",
        ylab = "Puissance nominale (kW)")

# 12. Créer des catégories de puissance
df_clean$categorie_puissance <- cut(
  df_clean$puissance_nominale,
  breaks = c(-Inf, 7, 22, 50, 150, 400, Inf),
  labels = c("Très faible", "Faible", "Moyenne", "Rapide", "Très rapide", "Extrême")
)

# 13. Compter les catégories de puissance
table(df_clean$categorie_puissance)

# 14. Pourcentage par catégorie
prop.table(table(df_clean$categorie_puissance)) * 100

# 15. Graphique des catégories de puissance
barplot(table(df_clean$categorie_puissance),
        main = "Répartition des catégories de puissance",
        xlab = "Catégorie de puissance",
        ylab = "Nombre de points de charge",
        las = 2)
