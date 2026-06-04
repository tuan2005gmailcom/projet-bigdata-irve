# ==============================================================================
# TABLEAUX CROISÉS ET TESTS D'INDÉPENDANCE DU KHI-DEUX
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. Création des variables propres nécessaires
# ------------------------------------------------------------------------------

# Présence de prise Combo CCS : Oui / Non
df_clean$combo_ccs_clean <- ifelse(
  tolower(as.character(df_clean$prise_type_combo_ccs)) %in% c("true", "1"),
  "Oui",
  "Non"
)

# Présence de prise Type 2 : Oui / Non
df_clean$prise_type_2_clean <- ifelse(
  tolower(as.character(df_clean$prise_type_2)) %in% c("true", "1"),
  "Oui",
  "Non"
)


# ------------------------------------------------------------------------------
# 2. Fonction propre pour tableau croisé + Khi-deux + mosaicplot
# ------------------------------------------------------------------------------

analyse_chi2 <- function(data, var1, var2, nom_graphique) {
  
  # Garder seulement les lignes où les deux variables existent
  sous_data <- data[
    !is.na(data[[var1]]) &
      !is.na(data[[var2]]),
  ]
  
  # Créer le tableau croisé
  tab <- table(sous_data[[var1]], sous_data[[var2]])
  
  # Supprimer les lignes/colonnes vides si nécessaire
  tab <- tab[rowSums(tab) > 0, colSums(tab) > 0]
  
  # Affichage du tableau croisé
  cat("\n============================================================\n")
  cat("TABLEAU CROISÉ :", var1, "x", var2, "\n")
  cat("============================================================\n")
  print(tab)
  
  # Pourcentages par ligne
  cat("\nPourcentages par ligne :\n")
  pourcentages <- round(prop.table(tab, margin = 1) * 100, 2)
  print(pourcentages)
  
  # Vérifier qu'il y a au moins 2 lignes et 2 colonnes
  if (nrow(tab) < 2 | ncol(tab) < 2) {
    cat("\nTest du Khi-deux impossible : pas assez de catégories.\n")
    return(NULL)
  }
  
  # Test du Khi-deux
  test <- suppressWarnings(chisq.test(tab))
  
  cat("\nTest du Khi-deux :\n")
  print(test)
  
  # Effectif attendu minimum
  min_expected <- min(test$expected)
  
  cat("\nEffectif attendu minimum :", min_expected, "\n")
  
  if (min_expected < 5) {
    cat("Attention : certains effectifs attendus sont inférieurs à 5. Le test est à interpréter avec prudence.\n")
  }
  
  # Cramer's V pour mesurer la force de la relation
  cramer_v <- sqrt(as.numeric(test$statistic) / (sum(tab) * (min(dim(tab)) - 1)))
  
  cat("\nCramer's V :", round(cramer_v, 4), "\n")
  
  # Sauvegarde du mosaicplot
  png(nom_graphique, width = 900, height = 650)
  
  mosaicplot(
    tab,
    main = paste("Lien entre", var1, "et", var2),
    xlab = var1,
    ylab = var2,
    color = TRUE,
    las = 2
  )
  
  dev.off()
  
  # Résultat résumé
  resultat <- data.frame(
    Variable_1 = var1,
    Variable_2 = var2,
    Chi_square = round(as.numeric(test$statistic), 3),
    ddl = as.numeric(test$parameter),
    p_value = test$p.value,
    Cramer_V = round(cramer_v, 4),
    Effectif_total = sum(tab),
    Min_effectif_attendu = round(min_expected, 3),
    Graphique = nom_graphique
  )
  
  return(resultat)
}


# ------------------------------------------------------------------------------
# 3. Analyses Khi-deux à réaliser
# ------------------------------------------------------------------------------

res1 <- analyse_chi2(
  data = df_clean,
  var1 = "gratuit_clean",
  var2 = "paiement_cb_clean",
  nom_graphique = "mosaicplot_gratuit_paiement_cb.png"
)

res2 <- analyse_chi2(
  data = df_clean,
  var1 = "condition_acces",
  var2 = "reservation_clean",
  nom_graphique = "mosaicplot_condition_acces_reservation.png"
)

res3 <- analyse_chi2(
  data = df_clean,
  var1 = "implantation_station",
  var2 = "condition_acces",
  nom_graphique = "mosaicplot_implantation_condition_acces.png"
)

res4 <- analyse_chi2(
  data = df_clean,
  var1 = "implantation_station",
  var2 = "paiement_cb_clean",
  nom_graphique = "mosaicplot_implantation_paiement_cb.png"
)

res5 <- analyse_chi2(
  data = df_clean,
  var1 = "combo_ccs_clean",
  var2 = "tarif_classe",
  nom_graphique = "mosaicplot_combo_ccs_tarif.png"
)

res6 <- analyse_chi2(
  data = df_clean,
  var1 = "prise_type_2_clean",
  var2 = "tarif_classe",
  nom_graphique = "mosaicplot_prise_type_2_tarif.png"
)


# ------------------------------------------------------------------------------
# 4. Résumé final des tests
# ------------------------------------------------------------------------------

resume_chi2 <- rbind(res1, res2, res3, res4, res5, res6)

resume_chi2

# ==============================================================================
# GRAPHES D'ASSOCIATION POUR TABLEAUX CROISÉS
# ==============================================================================

# ------------------------------------------------------------------------------
# Fonction : barplot en pourcentage + heatmap des résidus du Khi-deux
# ------------------------------------------------------------------------------

graphes_association <- function(data, var1, var2, prefixe) {
  
  # 1. Garder les lignes valides
  sous_data <- data[
    !is.na(data[[var1]]) &
      !is.na(data[[var2]]),
  ]
  
  # 2. Créer le tableau croisé
  tab <- table(sous_data[[var1]], sous_data[[var2]])
  tab <- tab[rowSums(tab) > 0, colSums(tab) > 0]
  
  # 3. Pourcentages par ligne
  prop_ligne <- prop.table(tab, margin = 1) * 100
  
  # ---------------------------------------------------------------------------
  # Graphique 1 : barplot empilé en pourcentage
  # ---------------------------------------------------------------------------
  
  png(paste0(prefixe, "_barplot_pourcentages.png"), width = 1000, height = 700)
  
  par(mar = c(9, 5, 4, 2))
  
  barplot(
    t(prop_ligne),
    beside = FALSE,
    main = paste("Répartition de", var2, "selon", var1),
    xlab = var1,
    ylab = "Pourcentage (%)",
    las = 2,
    col = rainbow(ncol(prop_ligne)),
    legend.text = colnames(prop_ligne),
    args.legend = list(
      x = "topright",
      cex = 0.8,
      bty = "n"
    )
  )
  
  dev.off()
  
  
  # ---------------------------------------------------------------------------
  # Graphique 2 : heatmap des résidus standardisés du Khi-deux
  # ---------------------------------------------------------------------------
  # Rouge = plus observé que prévu si les variables étaient indépendantes
  # Bleu = moins observé que prévu si les variables étaient indépendantes
  
  test <- suppressWarnings(chisq.test(tab))
  residus <- test$stdres
  
  png(paste0(prefixe, "_heatmap_residus_chi2.png"), width = 1000, height = 700)
  
  par(mar = c(8, 10, 4, 5))
  
  couleurs <- colorRampPalette(c("blue", "white", "red"))(100)
  
  image(
    x = 1:ncol(residus),
    y = 1:nrow(residus),
    z = t(residus[nrow(residus):1, ]),
    col = couleurs,
    axes = FALSE,
    main = paste("Résidus du Khi-deux :", var1, "x", var2),
    xlab = var2,
    ylab = var1
  )
  
  axis(
    1,
    at = 1:ncol(residus),
    labels = colnames(residus),
    las = 2,
    cex.axis = 0.8
  )
  
  axis(
    2,
    at = 1:nrow(residus),
    labels = rev(rownames(residus)),
    las = 2,
    cex.axis = 0.8
  )
  
  # Ajouter les valeurs des résidus dans les cases
  for (i in 1:nrow(residus)) {
    for (j in 1:ncol(residus)) {
      text(
        x = j,
        y = nrow(residus) - i + 1,
        labels = round(residus[i, j], 1),
        cex = 0.8
      )
    }
  }
  
  dev.off()
  
  # 4. Retourner les informations principales
  cramer_v <- sqrt(as.numeric(test$statistic) / (sum(tab) * (min(dim(tab)) - 1)))
  
  return(data.frame(
    Variable_1 = var1,
    Variable_2 = var2,
    Chi_square = round(as.numeric(test$statistic), 3),
    ddl = as.numeric(test$parameter),
    p_value = test$p.value,
    Cramer_V = round(cramer_v, 4),
    Graphique_pourcentages = paste0(prefixe, "_barplot_pourcentages.png"),
    Graphique_residus = paste0(prefixe, "_heatmap_residus_chi2.png")
  ))
}

# ==============================================================================
# APPLICATION DES GRAPHES D'ASSOCIATION
# ==============================================================================

g1 <- graphes_association(
  data = df_clean,
  var1 = "gratuit_clean",
  var2 = "paiement_cb_clean",
  prefixe = "assoc_gratuit_paiement_cb"
)

g2 <- graphes_association(
  data = df_clean,
  var1 = "implantation_station",
  var2 = "paiement_cb_clean",
  prefixe = "assoc_implantation_paiement_cb"
)

g3 <- graphes_association(
  data = df_clean,
  var1 = "implantation_station",
  var2 = "condition_acces",
  prefixe = "assoc_implantation_condition_acces"
)

g4 <- graphes_association(
  data = df_clean,
  var1 = "combo_ccs_clean",
  var2 = "tarif_classe",
  prefixe = "assoc_combo_ccs_tarif"
)

g5 <- graphes_association(
  data = df_clean,
  var1 = "prise_type_2_clean",
  var2 = "tarif_classe",
  prefixe = "assoc_prise_type_2_tarif"
)

resume_graphes_association <- rbind(g1, g2, g3, g4, g5)

resume_graphes_association
