# ==============================================================================
# SAE - MODÉLISATION DU PRIX DES VOITURES SUR LE MARCHÉ AMÉRICAIN
# Source des données : https://www.kaggle.com/datasets/hellbuoy/car-price-prediction
#
# Objectif : Aider Geely Auto (constructeur chinois) à comprendre les facteurs
# qui influencent les prix des voitures aux États-Unis, afin de définir une
# stratégie tarifaire compétitive pour son entrée sur le marché américain.
#
# Structure du script :
#   - PARTIE 0  : Configuration & Chargement des données
#   - PARTIE I  : Analyse exploratoire (EDA) + Régression linéaire multiple
#   - PARTIE II : Analyse de la Variance (ANOVA)
#   - PARTIE III: Sélection de variables (VIF, Stepwise, regsubsets, prévisions)
# ==============================================================================


# ==============================================================================
# PARTIE 0 — CONFIGURATION ET CHARGEMENT DES DONNÉES
# ==============================================================================

# --- Nettoyage de l'environnement ---
# On commence toujours par vider l'environnement R pour éviter les conflits
# avec des variables ou objets d'une session précédente.
rm(list = ls())

# --- Chargement des bibliothèques ---
# Chaque bibliothèque sert un rôle précis dans l'analyse.
library(ggplot2)     # Graphiques avancés et professionnels
library(gridExtra)   # Afficher plusieurs graphiques ggplot côte à côte
library(tidyverse)   # Ensemble d'outils de manipulation de données (dplyr, tidyr, etc.)
library(car)         # Calcul des VIF (Variance Inflation Factor) pour la colinéarité
library(MASS)        # Transformation de Box-Cox pour corriger la non-normalité
library(leaps)       # Sélection exhaustive de variables (regsubsets)
library(DescTools)   # Test de Scheffé pour les comparaisons multiples (ANOVA)

# --- Chargement du jeu de données ---
# On définit le répertoire de travail et on lit le fichier CSV.
# header = TRUE : la 1ère ligne contient les noms des colonnes.
# sep = ',' : le séparateur de colonnes est une virgule.
setwd("/drive/alumni/iut2502239/SD2/SEMESTRE4/Regression_R")
df <- read.csv(file = "CarPrice_Assignment.csv", header = TRUE, sep = ",")


# ==============================================================================
# PARTIE 0.1 — EXPLORATION INITIALE ET CONTRÔLE QUALITÉ DES DONNÉES
# ==============================================================================

# --- Aperçu global du jeu de données ---
# head() affiche les 6 premières lignes pour un premier aperçu visuel.
# dim() retourne le nombre de lignes et de colonnes : ici 205 lignes x 26 colonnes.
# summary() donne les statistiques descriptives pour chaque variable
# (min, max, moyenne, médiane, quartiles).
head(df)
dim(df)
summary(df)

# --- Vérification des types de variables ---
# str() affiche la structure du dataframe (type de chaque colonne : int, chr, num, etc.)
# sapply() applique la fonction class() sur chaque colonne pour confirmer les types.
str(df)
sapply(df, class)

# --- Dimensions détaillées ---
# On vérifie explicitement le nombre de colonnes et de lignes.
ncol(df)  # Résultat attendu : 26 variables
nrow(df)  # Résultat attendu : 205 observations

# --- Détection des valeurs manquantes ---
# is.na() crée une matrice TRUE/FALSE indiquant où sont les valeurs manquantes.
# colSums() somme les NA par colonne, et colMeans() calcule le % de NA.
# sort(..., decreasing = TRUE) trie du plus grand au plus petit pour repérer les pires colonnes.
# Résultat attendu : 0 valeur manquante dans ce jeu de données.
sort(colSums(is.na(df)),  decreasing = TRUE)  # Nombre de NA par colonne
sort(colMeans(is.na(df)) * 100, decreasing = TRUE)  # % de NA par colonne

# --- Détection des doublons ---
# duplicated() renvoie TRUE pour les lignes qui sont des copies exactes d'une ligne précédente.
# sum() compte le nombre de lignes dupliquées. Résultat attendu : 0 doublon.
sum(duplicated(df))


# ==============================================================================
# PARTIE I — ANALYSE EXPLORATOIRE (EDA) ET RÉGRESSION LINÉAIRE MULTIPLE
# ==============================================================================

# ==============================================================================
# I.1 — ANALYSE UNIVARIÉE : Distribution de la variable cible (price)
# ==============================================================================

# On ferme tous les graphiques précédents pour repartir sur une fenêtre propre.
while (!is.null(dev.list())) dev.off()

# Graphique 1 : Histogramme + Courbe de densité du prix
# geom_histogram() dessine les barres. aes(y = ..density..) normalise l'axe Y
# pour le superposer avec la densité. geom_density() trace la courbe lissée.
# label_number() supprime la notation scientifique (ex: 1e-04) sur l'axe Y.
p1 <- ggplot(df, aes(x = price)) +
  geom_histogram(aes(y = ..density..), bins = 30,
                 fill = "royalblue", color = "white", alpha = 0.7) +
  geom_density(color = "red", linewidth = 1) +
  scale_y_continuous(labels = scales::label_number()) +
  labs(title = "Distribution des Prix", x = "Prix ($)", y = "Densité") +
  theme_minimal()

# Graphique 2 : Boxplot pour détecter les valeurs aberrantes (outliers)
# outlier.shape = 8 : les outliers sont symbolisés par une étoile (*).
# theme(axis.text.x = element_blank()) : retire les étiquettes inutiles sur l'axe X.
p2 <- ggplot(df, aes(y = price)) +
  geom_boxplot(fill = "tomato", alpha = 0.6,
               outlier.color = "red", outlier.shape = 8) +
  labs(title = "Détection des Outliers", y = "Prix ($)") +
  theme_minimal() +
  theme(axis.text.x = element_blank())

# Affichage côte à côte : ncol = 2 place les deux graphiques sur une ligne.
grid.arrange(p1, p2, ncol = 2)

# --- Interprétation ---
# La distribution du prix est asymétrique à droite (skewness positif) :
# la majorité des voitures se vendent entre 5 000$ et 15 000$, mais quelques
# modèles de luxe dépassent les 30 000$ (visibles comme outliers sur le boxplot).
# La médiane (~10 000$) est plus fiable que la moyenne car elle est moins
# sensible à ces valeurs extrêmes.
# => Conclusion : il faudra surveiller l'impact des outliers sur le modèle.


# ==============================================================================
# I.2 — RELATIONS BIVARIÉES : Variables techniques vs Prix
# ==============================================================================

# Graphique 3 : Puissance (horsepower) vs Prix
# geom_smooth(method = "lm") trace la droite de régression linéaire avec
# son intervalle de confiance (se = TRUE) en zone grise.
# dollar_format() formate l'axe Y en dollars ($).
p3 <- ggplot(df, aes(x = horsepower, y = price)) +
  geom_point(color = "royalblue", alpha = 0.5, size = 2) +
  geom_smooth(method = "lm", formula = y ~ x, color = "red", se = TRUE) +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(title    = "Puissance (Horsepower) vs Prix",
       subtitle = "Corrélation positive forte",
       x = "Puissance (chevaux)", y = "Prix ($)") +
  theme_light(base_size = 11)

# Graphique 4 : Taille du moteur (enginesize) vs Prix
p4 <- ggplot(df, aes(x = enginesize, y = price)) +
  geom_point(color = "darkgreen", alpha = 0.5, size = 2) +
  geom_smooth(method = "lm", formula = y ~ x, color = "red", se = TRUE) +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(title    = "Taille du moteur vs Prix",
       subtitle = "Corrélation positive forte",
       x = "Cylindrée (Engine Size)", y = "Prix ($)") +
  theme_light(base_size = 11)

grid.arrange(p3, p4, ncol = 2)

# --- Interprétation ---
# Les deux nuages de points montrent une corrélation positive forte :
# plus la puissance ou la cylindrée augmente, plus le prix s'élève.
# La zone grise (IC à 95%) est étroite pour les faibles puissances (marché de masse)
# et s'élargit pour les fortes puissances (marché de luxe plus imprévisible).
# => Ces deux variables sont de bons candidats pour notre modèle de régression.


# ==============================================================================
# I.3 — VARIABLE QUALITATIVE : Impact du type de propulsion sur le prix
# ==============================================================================

# geom_boxplot(outlier.shape = NA) masque les points aberrants dans la boîte
# car geom_jitter() les affiche de toute façon de manière plus lisible.
# geom_jitter(width = 0.15) ajoute un décalage horizontal aléatoire faible
# pour éviter la superposition des points.
ggplot(df, aes(x = drivewheel, y = price, fill = drivewheel)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.4, color = "black") +
  scale_fill_manual(values = c("4wd" = "#E41A1C", "fwd" = "#377EB8", "rwd" = "#4DAF4A")) +
  labs(title    = "Impact du mode de propulsion sur le prix",
       subtitle = "Comparaison des segments de marché (4x4, Traction, Propulsion)",
       x = "Type de propulsion (4wd, fwd, rwd)", y = "Prix ($)") +
  theme_minimal() +
  theme(legend.position = "none")

# --- Interprétation ---
# fwd (traction avant) : marché de masse, prix bas et concentrés (~5 000$-10 000$).
# rwd (propulsion arrière) : segment premium, prix très dispersés (15 000$-45 000$+).
# 4wd (4 roues motrices) : segment intermédiaire, peu de représentants.
# => La variable drivewheel est un bon candidat pour une ANOVA (Partie II).


# ==============================================================================
# I.4 — RÉGRESSION LINÉAIRE MULTIPLE — MODÈLE COMPLET
# ==============================================================================

# --- Préparation des données quantitatives ---
# On sélectionne uniquement les colonnes numériques du dataframe.
# sapply(df, is.numeric) retourne un vecteur logique TRUE/FALSE pour chaque colonne.
# On retire car_ID car c'est un identifiant sans valeur prédictive.
df_quant <- df[, sapply(df, is.numeric)]
df_quant$car_ID <- NULL

# --- Matrice de dispersion (Scatter Plot Matrix) ---
# pairs() croise chaque variable numérique contre toutes les autres.
# Cela permet de visualiser d'un seul coup toutes les corrélations bivariées.
# Une tendance linéaire visible = bonne candidature pour la régression.
par(mfrow = c(1, 1))
par(mar  = c(1, 1, 1, 1))
pairs(df_quant)

# --- Modèle complet (toutes les variables quantitatives) ---
# lm(price ~ ., data = df_quant) : "price" est la variable réponse (Y),
# et "." signifie "toutes les autres colonnes" comme variables explicatives (X).
reg <- lm(price ~ ., data = df_quant)
summary(reg)

# --- Interprétation du summary() ---
# Estimate (colonnes des coefficients β) : effet de chaque variable sur le prix.
#   Ex: enginesize = +116.8 => chaque unité supplémentaire fait monter le prix de 116,8$.
# Std. Error : précision de l'estimation. Plus elle est grande, moins c'est fiable.
# R² = 0.8519 : le modèle explique 85,2% de la variation du prix. Score élevé !
# Residual Std. Error = 3186 => variance estimée des erreurs = 3186² ≈ 10 150 596.
# F-statistic = 78.05, p-value < 2.2e-16 : le modèle est globalement très significatif.
#   => On rejette H0 "tous les coefficients sont nuls". Au moins une variable est utile.


# ==============================================================================
# I.5 — ANALYSE DES RÉSIDUS DU MODÈLE COMPLET
# ==============================================================================

# --- Graphiques de diagnostic standard (4 en 1) ---
# which = c(1,2,4,5) sélectionne 4 graphiques diagnostics :
#   1. Residuals vs Fitted : vérifie la linéarité (résidus ~ 0 partout ?)
#   2. Normal Q-Q : vérifie la normalité des résidus (alignement sur la droite ?)
#   4. Cook's Distance : identifie les points influents (levier fort sur β)
#   5. Residuals vs Leverage : combine résidu et levier pour repérer les points aberrants
par(mfrow = c(2, 2))
plot(reg, which = c(1, 2, 4, 5))
par(mfrow = c(1, 1))

# --- Interprétation ---
# Q-Q Plot : les points s'écartent aux extrémités => queues lourdes, non-normalité.
# Cook's Distance : l'observation n°50 a un pic très élevé => influence disproportionnée.
# Residuals vs Leverage : le point n°50 est isolé, fort levier sur le modèle.
# => Décision : supprimer l'observation 50 pour stabiliser le modèle.


# ==============================================================================
# I.6 — SUPPRESSION DU POINT INFLUENT ET NOUVEAU MODÈLE
# ==============================================================================

# On mémorise la taille initiale du dataset.
n <- nrow(df_quant)

# On supprime la ligne 50 (le point aberrant identifié ci-dessus).
# On renumérote les lignes de 1 à (n-1) pour garder un index propre.
df_quant2 <- df_quant[-50, ]
rownames(df_quant2) <- seq(1, n - 1)

# On ajuste un nouveau modèle sur les données nettoyées.
reg2 <- lm(price ~ ., data = df_quant2)
summary(reg2)

# Nouveau diagnostic des résidus après suppression du point 50.
par(mfrow = c(2, 2))
plot(reg2, which = c(1, 2, 4, 5))
par(mfrow = c(1, 1))

# --- Interprétation ---
# R² ajusté reste à ~0.85 (bonne stabilité).
# Les points 49 et 128 deviennent les nouveaux points les plus atypiques.
# On va maintenant nettoyer de manière systématique via une boucle.


# ==============================================================================
# I.7 — NETTOYAGE ITÉRATIF PAR BOUCLE (Résidus standardisés > 3.5)
# ==============================================================================

# --- Proportion initiale de résidus aberrants ---
# rstandard() calcule les résidus standardisés (en écarts-types).
# Un résidu > 2 en valeur absolue est potentiellement atypique.
# Selon la loi normale, on accepte ~5% de ces valeurs naturellement.
reg3       <- lm(price ~ ., data = df_quant2)
res_std3   <- rstandard(reg3)
prop_avant <- length(which(abs(res_std3) > 2)) / nrow(df_quant2) * 100
cat("Proportion d'aberrants avant boucle :", round(prop_avant, 2), "%\n")

# --- Boucle de nettoyage itératif ---
# Pourquoi le seuil 3.5 (et pas 2) ?
# Supprimer tous ceux > 2 serait trop agressif (~5% des données perdues inutilement).
# Un résidu > 3.5 est extrêmement rare (hors de 99.9% de la distribution normale).
# On supprime UN point à la fois, puis on recalcule le modèle => plus rigoureux.
repeat {
  # Nouveau modèle sur les données nettoyées à l'étape précédente
  reg_boucle <- lm(price ~ ., data = df_quant2)

  # Résidus standardisés recalculés sur ce nouveau modèle
  res_std    <- rstandard(reg_boucle)

  # Index du point avec le résidu le plus extrême en valeur absolue
  idx_pire   <- which.max(abs(res_std))

  # Condition d'arrêt : si même le pire résidu est acceptable, on s'arrête
  if (abs(res_std[idx_pire]) <= 3.5) break

  # Sinon, on supprime le point et on renumérote
  df_quant2            <- df_quant2[-idx_pire, ]
  rownames(df_quant2)  <- 1:nrow(df_quant2)
}

# Bilan des suppressions
n_final     <- nrow(df_quant2)
n_initial   <- nrow(df_quant)
nb_supprime <- n_initial - n_final
cat("Nombre de données supprimées :", nb_supprime, "\n")


# ==============================================================================
# I.8 — MODÈLE FINAL NETTOYÉ (reg_clean)
# ==============================================================================

# Ce modèle est ajusté sur le dataset propre (sans outliers extrêmes).
reg_clean <- lm(price ~ ., data = df_quant2)
summary(reg_clean)

# Diagnostic des résidus du modèle nettoyé (les 4 graphiques standards)
par(mfrow = c(2, 2))
plot(reg_clean, which = c(1, 2, 4, 5))
par(mfrow = c(1, 1))

# --- Améliorations constatées ---
# R² passe de 0.8519 à 0.8827 (+3%) : meilleur pouvoir explicatif.
# Residual Std. Error chute de 3186 à 2364 : prédictions plus précises.
# F-statistic monte de 78.05 à 95.72 : relation globale plus robuste.
# Plus de variables sont significatives (ex: boreratio passe de p=0.41 à p=0.009).


# ==============================================================================
# I.9 — VÉRIFICATION DE LA NORMALITÉ DES RÉSIDUS
# ==============================================================================

# QQ-Plot : si les points suivent la droite rouge, les résidus sont gaussiens.
qqnorm(residuals(reg_clean), main = "QQ-Plot des résidus")
qqline(residuals(reg_clean), col = "red")

# Test de Shapiro-Wilk : H0 = "les résidus suivent une loi normale".
# Si p-value > 0.05 => on ne rejette pas H0 => normalité acceptable.
# Note : on appelle le test une seule fois (appel en double = redondant).
shapiro_result <- shapiro.test(residuals(reg_clean))
print(shapiro_result)

# Résidus vs Valeurs fittées (vérification de l'homoscédasticité)
# abline(h = 0) trace la ligne de référence à zéro.
# Si les points sont bien répartis autour de cette ligne => homoscédasticité validée.
plot(fitted(reg_clean), residuals(reg_clean),
     main = "Résidus vs Fitted (Clean)",
     xlab = "Valeurs prédites", ylab = "Résidus")
abline(h = 0, col = "red")


# ==============================================================================
# I.10 — INTERVALLES DE CONFIANCE ET VARIABLES SIGNIFICATIVES
# ==============================================================================

# --- Intervalles de confiance à 95% des coefficients ---
# confint() calcule la borne inférieure et supérieure de chaque coefficient β.
# Règle : si l'intervalle NE contient PAS 0 => variable statistiquement significative.
confint(reg_clean, level = 0.95)

# --- Variables significatives au seuil de 5% ---
# summary()$coefficients retourne la matrice des résultats du modèle.
# La colonne 4 contient les p-values. On filtre celles < 0.05 (seuil de 5%).
summary_clean        <- summary(reg_clean)
variables_sig        <- summary_clean$coefficients[summary_clean$coefficients[, 4] < 0.05, ]
cat("\nVariables significatives au seuil de 5% :\n")
print(variables_sig)

# Variables avec impact POSITIF sur le prix : enginesize, carwidth,
#   compressionratio, wheelbase, peakrpm.
# Variables avec impact NÉGATIF sur le prix : stroke, carlength.


# ==============================================================================
# I.11 — GRAPHIQUE : VALEURS OBSERVÉES vs PRÉDITES
# ==============================================================================

# fitted(reg_clean) retourne les valeurs prédites par le modèle pour chaque obs.
# abline(a=0, b=1) trace la droite d'identité Y = X.
# Si tous les points sont sur cette droite => modèle parfait.
plot(df_quant2$price, fitted(reg_clean),
     main = "Valeurs Observées vs Prédites",
     xlab = "Prix Réels (Observés)",
     ylab = "Prix Prédits par le Modèle",
     pch = 19, col = "blue")
abline(a = 0, b = 1, col = "red", lwd = 2)

# --- Interprétation ---
# Les points bleus sont proches de la ligne rouge pour les prix 5 000$-20 000$.
# Légère dispersion au-delà de 30 000$ (voitures de luxe, plus imprévisibles).
# => Le modèle est globalement très fiable pour le marché de masse.


# ==============================================================================
# I.12 — PRÉVISIONS SUR 10 NOUVELLES OBSERVATIONS
# ==============================================================================

# On sélectionne les 10 dernières observations du dataset original (lignes 196-205).
# names(df_quant) assure qu'on garde les mêmes colonnes que le modèle reg_clean.
new_obs <- df[196:205, names(df_quant)]
y_reel  <- new_obs$price

# --- Intervalle de Confiance (IC Espérance) ---
# interval = "confidence" : fourchette pour le PRIX MOYEN d'un groupe de voitures
# similaires. Plus étroit car il estime une moyenne (moins d'incertitude).
ic_esperance <- predict(reg_clean, newdata = new_obs,
                        interval = "confidence", level = 0.95)

# --- Intervalle de Prévision ---
# interval = "prediction" : fourchette pour le PRIX D'UNE VOITURE INDIVIDUELLE.
# Plus large car il intègre aussi la variabilité naturelle d'un seul véhicule.
ic_prevision <- predict(reg_clean, newdata = new_obs,
                        interval = "prediction", level = 0.95)

# --- MSE (Mean Squared Error) ---
# Mesure la qualité prédictive : moyenne des erreurs au carré.
# RMSE = √MSE donne l'erreur moyenne en dollars (plus interprétable).
predictions <- ic_prevision[, "fit"]
mse_val     <- mean((y_reel - predictions)^2)

print("--- Intervalles de Confiance (Moyenne) ---")
print(ic_esperance)
print("--- Intervalles de Prévision (Individuel) ---")
print(ic_prevision)
cat("\nMSE du modèle :", mse_val, "\n")
cat("RMSE du modèle :", sqrt(mse_val), "$ en moyenne par voiture\n")


# ==============================================================================
# I.13 — GRAPHIQUE DE VALIDATION : PRIX RÉELS vs PRÉDICTIONS
# ==============================================================================

# On trie par prix prédit croissant pour une lecture fluide de la courbe.
ordre       <- order(ic_prevision[, "fit"])
y_reel_tri  <- y_reel[ordre]
fit_tri     <- ic_prevision[ordre, "fit"]

# Création de la zone de tracé (type = "n" = tracé vide, axes seulement)
plot(1:10, fit_tri, type = "n",
     ylim = c(min(ic_prevision[, "lwr"]), max(ic_prevision[, "upr"])),
     main = "Validation du Modèle : Prévoir le juste prix",
     xlab = "10 voitures tests (de la moins chère à la plus chère)",
     ylab = "Prix en Dollars ($)",
     xaxt = "n")
axis(1, at = 1:10, labels = paste("Modèle", 1:10))

# Zone bleue : Intervalle de Prévision (incertitude pour une voiture précise)
# polygon() dessine un polygone fermé. rgb() définit la couleur avec transparence (0.2).
polygon(c(1:10, rev(1:10)),
        c(ic_prevision[ordre, "lwr"], rev(ic_prevision[ordre, "upr"])),
        col = rgb(0.1, 0.5, 0.8, 0.2), border = NA)

# Zone rouge : Intervalle de Confiance (incertitude sur le prix moyen du segment)
polygon(c(1:10, rev(1:10)),
        c(ic_esperance[ordre, "lwr"], rev(ic_esperance[ordre, "upr"])),
        col = rgb(1, 0, 0, 0.3), border = NA)

# Ligne noire : L'estimation centrale (prix "idéal" calculé par le modèle)
lines(1:10, fit_tri, col = "black", lwd = 2, lty = 1)

# Croix bleues : Prix réels (ce qui s'est vraiment passé sur le marché)
points(1:10, y_reel_tri, pch = 4, col = "darkblue", lwd = 2, cex = 1.2)

legend("topleft",
       legend = c("Prix Réel (Marché)",
                  "Estimation (Prédiction idéale)",
                  "IC Espérance (Zone sécurité - Moyenne)",
                  "IC Prévision (Zone sécurité - Voiture seule)"),
       col = c("darkblue", "black", "red", "royalblue"),
       pch = c(4, NA, 15, 15),
       lty = c(NA, 1, NA, NA),
       pt.cex = 1.5, bty = "n", cex = 0.8)

# --- Interprétation ---
# VALIDATION RÉUSSIE : Toutes les croix bleues (prix réels) se trouvent
# à l'intérieur de la zone bleue (IC Prévision à 95%).
# La ligne noire suit bien la tendance des prix réels même pour les hauts gammes.
# => Conclusion : le modèle est prêt pour fixer les prix des futurs modèles Geely Auto.


# ==============================================================================
# PARTIE II — ANALYSE DE LA VARIANCE (ANOVA)
# Impact du nombre de portes (doornumber) sur le prix
# ==============================================================================

# Objectif : Vérifier si le nombre de portes (2 ou 4) influence le prix de manière
# statistiquement significative. Pour Geely Auto, cela permet de savoir si ce
# critère justifie une différence de tarification.


# ==============================================================================
# II.1 — PRÉPARATION ET ENCODAGE DES DONNÉES ANOVA
# ==============================================================================

# On repart des données brutes originales (df) pour l'ANOVA.
# On sélectionne uniquement les colonnes nécessaires : price et doornumber.
data_anova <- df[, c("price", "doornumber")]

# Encodage en facteur : indique à R que 'doornumber' est une variable catégorielle
# (deux modalités : "two" et "four"), et non un nombre à calculer.
data_anova$doornumber <- as.factor(data_anova$doornumber)


# ==============================================================================
# II.2 — ANALYSE GRAPHIQUE EXPLORATOIRE (Boxplot)
# ==============================================================================

# Boxplot comparatif des prix selon le nombre de portes.
# Première impression visuelle avant le test statistique formel.
boxplot(price ~ doornumber,
        data  = data_anova,
        main  = "Comparaison des prix par nombre de portes",
        xlab  = "Nombre de portes",
        ylab  = "Prix ($)",
        col   = c("lightblue", "lightgreen"))

# --- Interprétation ---
# Les deux boîtes sont quasiment à la même hauteur => visuellement, le prix
# semble peu différent entre 2 et 4 portes. L'ANOVA va confirmer ou infirmer.


# ==============================================================================
# II.3 — STATISTIQUES DE L'ANOVA (Quantités importantes)
# ==============================================================================

# nk : effectifs par groupe (nombre de voitures à 2 portes, à 4 portes).
# K  : nombre de modalités (ici K = 2 : "two" et "four").
# n  : taille totale de l'échantillon.
# moyk : moyennes de prix par groupe.
# moyt : moyenne globale de tous les prix.
nk   <- as.numeric(table(data_anova$doornumber))
K    <- length(nk)
n    <- sum(nk)
moyk <- as.numeric(tapply(data_anova$price, data_anova$doornumber, mean))
moyt <- mean(data_anova$price)

# Vérification mathématique de la cohérence :
# La moyenne globale doit égaler la moyenne pondérée des groupes.
# Si les deux valeurs sont identiques, les calculs sont corrects.
cat("Moyenne globale :", moyt, "\n")
cat("Vérification pondérée :", sum(moyk * nk) / n, "\n")


# ==============================================================================
# II.4 — TEST ANOVA (Décomposition de la variance)
# ==============================================================================

# aov() ajuste le modèle ANOVA.
# H0 : les moyennes de prix sont égales pour 2 portes et 4 portes.
# H1 : au moins un groupe a une moyenne différente.
price.aov <- aov(price ~ doornumber, data = data_anova)
summary(price.aov)

# --- Interprétation initiale ---
# Si p-value > 0.05 => on ne rejette pas H0 => le nombre de portes n'a pas
# d'impact significatif sur le prix. => C'est ce qu'on observe ici sur les données brutes.


# ==============================================================================
# II.5 — VÉRIFICATION DES HYPOTHÈSES ET NETTOYAGE DES DONNÉES ANOVA
# ==============================================================================

# Diagnostic des résidus de l'ANOVA.
# Graphique 1 (Residuals vs Fitted) : vérifie l'homoscédasticité (variance constante).
# Graphique 2 (Normal Q-Q) : vérifie la normalité des erreurs.
par(mfrow = c(1, 2))
plot(price.aov, which = c(1, 2))
par(mfrow = c(1, 1))

# --- Nettoyage des points influents ---
# On supprime les observations 50, 73 et 74, identifiées graphiquement comme
# des points atypiques qui "tirent" les moyennes de groupe artificiellement.
# Justification : L'ANOVA compare des moyennes. Un seul véhicule hors-norme
# (supercar à 45 000$) fausse la moyenne de son groupe et invalide les conclusions.
data_anova <- data_anova[-c(50, 73, 74), ]
rownames(data_anova) <- 1:nrow(data_anova)

# --- Nettoyage complémentaire via résidus standardisés ---
# On ajuste un modèle temporaire pour calculer les résidus standardisés.
# Les observations avec |résidu| > 2.5 sont retirées (moins de 5% naturellement).
anova_temp  <- aov(price ~ doornumber, data = data_anova)
res_std     <- rstandard(anova_temp)
prop_anova  <- length(which(abs(res_std) > 2)) / nrow(data_anova) * 100
cat("Proportion de résidus hors-norme (>2) :", round(prop_anova, 2), "%\n")

# Indices des observations à supprimer (résidu > 2.5)
a_supprimer <- which(abs(res_std) > 2.5)

# On supprime uniquement si des points à supprimer existent (condition de sécurité)
data_anova_clean <- data_anova
if (length(a_supprimer) > 0) {
  data_anova_clean              <- data_anova_clean[-a_supprimer, ]
  rownames(data_anova_clean)    <- 1:nrow(data_anova_clean)
}

# ANOVA finale sur les données nettoyées
anova_final <- aov(price ~ doornumber, data = data_anova_clean)
summary(anova_final)

# --- Interprétation après nettoyage ---
# p-value = 0.0208 < 0.05 => on rejette H0.
# Conclusion : le nombre de portes influence significativement le prix
# sur le marché américain (une fois les voitures de luxe extrêmes écartées).
# Les voitures à 2 portes coûtent en moyenne ~1 793$ de moins que les 4 portes.


# ==============================================================================
# II.6 — COMPARAISONS MULTIPLES POST-HOC (Tukey & Scheffé)
# ==============================================================================

# Ces tests identifient précisément QUELS groupes sont différents l'un de l'autre.
# Pour 2 groupes, il n'y a qu'une seule paire à comparer (two vs four).

# --- Test de Tukey (HSD - Honestly Significant Difference) ---
# Plus puissant que Scheffé quand on compare uniquement des paires.
# L'intervalle de confiance graphique NE doit PAS contenir 0 pour être significatif.
tukey_test <- TukeyHSD(anova_final)
print(tukey_test)
plot(tukey_test, col = "blue")

# --- Test de Scheffé ---
# Plus conservateur (rigoureux) que Tukey. Utilisé pour valider la conclusion.
# Si les deux tests donnent la même p-value => conclusion très solide.
scheffe_test <- ScheffeTest(anova_final)
print(scheffe_test)

# --- Interprétation combinée ---
# Les deux tests confirment : diff = -1793.03$, p-value ≈ 0.021.
# Les véhicules à 2 portes sont en moyenne ~1 793$ moins chers que les 4 portes.
# L'IC de Tukey est entièrement dans les valeurs négatives => pas d'ambiguïté.
# => Décision stratégique pour Geely Auto : le nombre de portes est un levier
#    de tarification réel. Les 4 portes peuvent justifier un prix plus élevé.


# ==============================================================================
# PARTIE III — SÉLECTION DE VARIABLES ET MODÈLES DE PRÉVISION
# ==============================================================================

# Objectif : Trouver le modèle le plus parcimonieux (simple et efficace) pour
# prédire le prix avec un minimum de variables mais un maximum de précision.


# ==============================================================================
# III.1 — DÉTECTION DE LA MULTICOLINÉARITÉ (VIF)
# ==============================================================================

# Le VIF (Variance Inflation Factor) mesure à quel point une variable est
# redondante avec les autres. Formule : VIF_i = 1 / (1 - R²_i).
# Seuils : VIF > 5-10 = problème, VIF > 10 = sévère (variable à retirer).
# Garder des variables colinéaires rend les coefficients instables et peu fiables.

# On repart du modèle nettoyé reg_clean.
vif_results <- vif(reg_clean)
print(sort(vif_results, decreasing = TRUE))

# --- Étape 1 : Suppression de 'citympg' ---
# citympg (VIF ≈ 27) et highwaympg (VIF ≈ 24) apportent la même information :
# la consommation de carburant. Garder les deux crée une redondance sévère.
# On retire citympg (consommation en ville) et on conserve highwaympg (autoroute).
reg_v2 <- update(reg_clean, . ~ . - citympg)
print(sort(vif(reg_v2), decreasing = TRUE))

# --- Étape 2 : Suppression de 'curbweight' ---
# curbweight (VIF > 16) est redondant avec carlength, carwidth et enginesize.
# Une voiture lourde est presque toujours une voiture grande avec un gros moteur.
# => curbweight n'apporte pas d'information unique sur le prix.
reg_v3 <- update(reg_v2, . ~ . - curbweight)
print(sort(vif(reg_v3), decreasing = TRUE))

# --- Résultat ---
# Après ces 2 suppressions, tous les VIF passent sous le seuil de 10.
# Le modèle reg_v3 est maintenant "sain" : chaque variable est indépendante.


# ==============================================================================
# III.2 — SÉLECTION PAS-À-PAS (Stepwise — Critère AIC)
# ==============================================================================

# step() automatise la sélection de variables en minimisant le critère AIC.
# AIC (Akaike Information Criterion) : pénalise les modèles complexes.
# direction = "both" : l'algorithme peut AJOUTER ou RETIRER des variables
# à chaque étape (combinaison Forward + Backward pour plus de flexibilité).
reg_final <- step(reg_v3, direction = "both")
summary(reg_final)

# Diagnostic des résidus du modèle sélectionné par Stepwise
par(mfrow = c(2, 2))
plot(reg_final, which = c(1, 2, 4, 5))
par(mfrow = c(1, 1))

# --- Résultat ---
# AIC initial : 3251.84. La variable 'boreratio' est éliminée (AIC tombe à 3251.04).
# R² ajusté final ≈ 0.8524 : on explique 85.2% de la variance du prix.
# La variable la plus significative : enginesize (***) et stroke (**).


# ==============================================================================
# III.3 — RECHERCHE EXHAUSTIVE (regsubsets — BIC, R² ajusté, Cp de Mallows)
# ==============================================================================

# regsubsets() teste TOUTES les combinaisons possibles de variables et sélectionne
# le meilleur modèle pour chaque taille (1 variable, 2 variables, ..., nvmax variables).
# nvmax = nombre maximum de variables à tester = toutes les colonnes sauf price.

# On utilise le modèle reg_v3 comme base (données propres sans colinéarité).
df_exhaustive <- reg_v3$model
best_models   <- regsubsets(price ~ .,
                            data  = df_exhaustive,
                            nvmax = ncol(df_exhaustive) - 1)
res_sum <- summary(best_models)

# --- Critères de sélection du nombre optimal de variables ---
# which.max(rsq)    : R² toujours croissant, favorise les grands modèles.
# which.max(adjr2)  : R² ajusté, meilleur équilibre précision / complexité.
# which.min(bic)    : BIC pénalise fortement la complexité => modèle plus simple.
# which.min(cp)     : Cp de Mallows, équilibre biais / variance.
best_r2adj <- which.max(res_sum$adjr2)  # => ~12 variables
best_bic   <- which.min(res_sum$bic)    # => 6 variables
best_cp    <- which.min(res_sum$cp)     # => 8 variables

cat("Nb variables optimal (R² Ajusté) :", best_r2adj, "\n")
cat("Nb variables optimal (BIC)        :", best_bic,   "\n")
cat("Nb variables optimal (Cp Mallows) :", best_cp,    "\n")

# Affichage des variables retenues par le critère R² ajusté
coef(best_models, best_r2adj)

# --- Graphiques des damiers de sélection ---
# Les cases noires = variables retenues. Plus on monte => meilleur score.
# enginesize et carwidth apparaissent systématiquement en haut => "piliers" du prix.
par(mfrow = c(2, 2))
plot(best_models, scale = "adjr2", main = "R² Ajusté")
plot(best_models, scale = "bic",   main = "BIC")
plot(best_models, scale = "Cp",    main = "Cp de Mallows")
par(mfrow = c(1, 1))


# ==============================================================================
# III.4 — MODÈLE DE PRÉVISION FINAL (Critère BIC — 6 variables)
# ==============================================================================

# Justification du choix du BIC :
# 1. Parcimonie : 6 variables vs 12 (R²adj) ou 8 (Cp) => modèle plus simple.
# 2. Robustesse : moins de risque de sur-apprentissage (overfitting).
# 3. Interprétabilité : Geely Auto peut piloter 6 leviers facilement.
# 4. Performance validée : R² ajusté ≈ 84.7% avec seulement 6 variables.

reg_prev <- lm(price ~ carwidth + enginesize + stroke +
                 compressionratio + horsepower + peakrpm,
               data = df_exhaustive)
summary(reg_prev)

# Toutes les 6 variables sont hautement significatives (*** ou **).
# enginesize et carwidth restent les facteurs dominants.


# ==============================================================================
# III.5 — MODÈLE DE MODÉLISATION (Critère Cp de Mallows — 8 variables)
# ==============================================================================

# Justification du choix du Cp de Mallows :
# Il cherche le meilleur équilibre biais/variance. Avec 8 variables, il offre
# une précision légèrement supérieure au BIC (6 var) tout en restant parcimonieux.

reg_model <- lm(price ~ wheelbase + carwidth + enginesize + stroke +
                  compressionratio + horsepower + peakrpm + highwaympg,
                data = df_exhaustive)
summary(reg_model)

# R² ajusté ≈ 0.8497 : ~85% de la variance expliquée.
# enginesize (***), carwidth (**), compressionratio (***), peakrpm (**) très signif.
# wheelbase et highwaympg ont p > 0.05 mais le Cp les juge utiles pour stabiliser
# la précision globale (réduction du biais de prédiction).


# ==============================================================================
# III.6 — VÉRIFICATION FINALE (Résidus + VIF) POUR LES DEUX MODÈLES
# ==============================================================================

# --- Diagnostic des résidus : Modèle BIC (reg_prev) ---
# On vérifie que les 4 hypothèses de la régression linéaire sont respectées.
vif(reg_prev)
par(mfrow = c(2, 2))
plot(reg_prev)
par(mfrow = c(1, 1))

# --- Diagnostic des résidus : Modèle Cp de Mallows (reg_model) ---
vif(reg_model)
par(mfrow = c(2, 2))
plot(reg_model)
par(mfrow = c(1, 1))

# --- Test VIF final : Modèle Cp de Mallows ---
# On vérifie qu'aucune des 8 variables n'a un VIF > 10 (seuil de colinéarité).
vif_cp_final <- vif(reg_model)
print("--- Scores VIF : Modèle Cp de Mallows (8 variables) ---")
print(sort(vif_cp_final, decreasing = TRUE))

if (all(vif_cp_final < 10)) {
  cat("\n✓ Toutes les variables ont un VIF < 10.",
      "\nLe modèle Cp est sain : aucun problème de multicolinéarité.\n")
} else {
  cat("\n⚠ Certaines variables dépassent le seuil de 10.",
      "\nUne simplification supplémentaire est nécessaire.\n")
}

# --- Test VIF final : Modèle BIC ---
# Même vérification pour le modèle BIC (6 variables).
vif_bic_final <- vif(reg_prev)
print("--- Scores VIF : Modèle BIC (6 variables) ---")
print(sort(vif_bic_final, decreasing = TRUE))

if (all(vif_bic_final < 10)) {
  cat("\n✓ Toutes les variables ont un VIF < 10.",
      "\nLe modèle BIC est sain : aucun problème de multicolinéarité.\n")
} else {
  cat("\n⚠ Certaines variables dépassent le seuil de 10.",
      "\nUne simplification supplémentaire est nécessaire.\n")
}


# ==============================================================================
# III.7 — PRÉVISIONS FINALES ET COMPARAISON DES MODÈLES (MSE)
# ==============================================================================

# On utilise les 10 mêmes observations tests que la Partie I (lignes 196-205).
new_obs_p3 <- df[196:205, ]
y_reel_p3  <- df[196:205, "price"]

# --- Calcul des intervalles de prévision ---
# predict() retourne les colonnes : fit (prédiction), lwr (borne inf), upr (borne sup).
ic_prev_bic <- predict(reg_prev,  newdata = new_obs_p3, interval = "prediction", level = 0.95)
ic_prev_cp  <- predict(reg_model, newdata = new_obs_p3, interval = "prediction", level = 0.95)

# --- Calcul des MSE pour comparer les deux modèles ---
# MSE = moyenne des (prix réel - prix prédit)². Plus c'est bas, mieux c'est.
mse_bic <- mean((y_reel_p3 - ic_prev_bic[, "fit"])^2)
mse_cp  <- mean((y_reel_p3 - ic_prev_cp[,  "fit"])^2)

print("--- Prévisions modèle BIC (6 variables) ---")
print(ic_prev_bic)
print("--- Prévisions modèle Cp Mallows (8 variables) ---")
print(ic_prev_cp)

cat("\nComparaison des performances (MSE) :\n")
cat("  MSE modèle BIC (6 var)       :", round(mse_bic, 0), "\n")
cat("  MSE modèle Cp Mallows (8 var):", round(mse_cp,  0), "\n")
cat("  MSE modèle complet           :", round(mse_val, 0), "\n")

# --- Interprétation finale ---
# Modèle complet : MSE ≈ 4 802 240 (le plus précis mathématiquement)
# Modèle Cp      : MSE ≈ 5 015 985 (très proche, avec seulement 8 variables)
# Modèle BIC     : MSE ≈ 5 583 269 (le plus simple, légèrement moins précis)
#
# => RECOMMANDATION : Modèle Cp de Mallows (8 variables).
#    Précision quasi identique au modèle complet, sans multicolinéarité,
#    plus facile à opérer commercialement pour Geely Auto.


# ==============================================================================
# III.8 — GRAPHIQUES COMPARATIFS FINAUX (BIC vs Cp de Mallows)
# ==============================================================================

# On affiche les deux graphiques de validation côte à côte.
par(mfrow = c(1, 2))

# --- Graphique 1 : Validation du Modèle BIC (6 variables) ---
# On trie par prix prédit pour avoir une courbe lisse.
ordre_bic  <- order(ic_prev_bic[, "fit"])
y_reel_bic <- y_reel_p3[ordre_bic]
fit_bic    <- ic_prev_bic[ordre_bic, "fit"]

plot(1:10, fit_bic, type = "n",
     ylim = c(min(ic_prev_bic[, "lwr"]), max(ic_prev_bic[, "upr"])),
     main = "Validation : Modèle BIC (6 var)",
     xlab = "10 voitures tests", ylab = "Prix ($)", xaxt = "n")
axis(1, at = 1:10, labels = paste("M", 1:10))

polygon(c(1:10, rev(1:10)),
        c(ic_prev_bic[ordre_bic, "lwr"], rev(ic_prev_bic[ordre_bic, "upr"])),
        col = rgb(0.1, 0.5, 0.8, 0.2), border = NA)
lines(1:10,  fit_bic,    col = "black",   lwd = 2)
points(1:10, y_reel_bic, pch = 4, col = "darkblue", lwd = 2)
legend("topleft", legend = c("Réel", "Prédit", "Zone Prév."),
       col = c("darkblue", "black", "royalblue"),
       pch = c(4, NA, 15), bty = "n", cex = 0.7)

# --- Graphique 2 : Validation du Modèle Cp de Mallows (8 variables) ---
ordre_cp  <- order(ic_prev_cp[, "fit"])
y_reel_cp <- y_reel_p3[ordre_cp]
fit_cp    <- ic_prev_cp[ordre_cp, "fit"]

plot(1:10, fit_cp, type = "n",
     ylim = c(min(ic_prev_cp[, "lwr"]), max(ic_prev_cp[, "upr"])),
     main = "Validation : Modèle Cp (8 var)",
     xlab = "10 voitures tests", ylab = "Prix ($)", xaxt = "n")
axis(1, at = 1:10, labels = paste("M", 1:10))

polygon(c(1:10, rev(1:10)),
        c(ic_prev_cp[ordre_cp, "lwr"], rev(ic_prev_cp[ordre_cp, "upr"])),
        col = rgb(0.1, 0.5, 0.8, 0.2), border = NA)
lines(1:10,  fit_cp,    col = "black",   lwd = 2)
points(1:10, y_reel_cp, pch = 4, col = "darkblue", lwd = 2)
legend("topleft", legend = c("Réel", "Prédit", "Zone Prév."),
       col = c("darkblue", "black", "royalblue"),
       pch = c(4, NA, 15), bty = "n", cex = 0.7)

# Retour à l'affichage standard
par(mfrow = c(1, 1))

# --- Interprétation finale ---
# Les deux graphiques sont quasi identiques
# Les 6 variables du BIC constituent le noyau dur commun aux deux modèles.
# Elles expliquent ~84% du prix. Les 2 variables supplémentaires du Cp (wheelbase
# et highwaympg) n'apportent qu'un gain visuel minimal (<1% de précision).
# => Les 6 variables clés suffisent à décrire la formation des prix du marché US.
# => Pour Geely Auto : concentrer la stratégie tarifaire sur enginesize, carwidth,
#    horsepower, stroke, compressionratio et peakrpm est amplement suffisant.

