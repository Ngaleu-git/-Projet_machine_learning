# ==============================================================================
# SAE 4-02 | BUT Science des Données – Université d'Avignon
# Analyse Multivariée : Forza Horizon Cars
# Auteur : Ngaleu Trésor
# ==============================================================================

library(FactoMineR)
library(factoextra)
library(RColorBrewer)
library(stringr)
library(dplyr)
library(ggplot2)
library(corrplot)
library(tidyr)
library(scales)

# ==============================================================================
# 1. IMPORTATION
# ==============================================================================
#Description du jeu de donnée :https://www.kaggle.com/datasets/deepcontractor/froza-horizon-5-cars-dataset

setwd("/drive/alumni/iut2502239/SD2/SEMESTRE4/SAE-Reporting d’une analyse multivariée")
car <- read.csv(file = "Forza_Horizon_Cars.csv", sep = ",", header = TRUE)

View(car)
head(car)
dim(car)   # 539 lignes x 22 colonnes
str(car)   # Plusieurs variables numériques importées en character -> nettoyage nécessaire

# ==============================================================================
# 2. NETTOYAGE (DATA WRANGLING)
# ==============================================================================

car_clean <- car

# Remplacement des valeurs "info_not_found" par NA
car_clean[car_clean == "info_not_found"] <- NA

# Suppression des unités textuelles et conversion en numérique
car_clean <- car_clean %>%
  mutate(
    In_Game_Price = as.numeric(str_replace_all(In_Game_Price, ",", "")),
    Weight_lbs    = as.numeric(str_replace_all(Weight_lbs,    ",", "")),
    Top_Speed     = as.numeric(str_remove(Top_Speed,  " Mph")),
    X0.60_Mph     = as.numeric(str_remove(X0.60_Mph,  "s")),
    X0.100_Mph    = as.numeric(str_remove(X0.100_Mph, "s")),
    g.force       = as.numeric(str_remove(g.force,    " g"))
  ) %>%
  mutate(across(
    c(speed, handling, acceleration, launch, braking, Offroad,
      Stock_Rating, Horse_Power),
    as.numeric
  ))

# Diagnostic des valeurs manquantes
sort(colSums(is.na(car_clean)), decreasing = TRUE)

#Descritpion du jeu de donnée brute 
summary(car_clean)
#Interpretation:
#le résumé statistique du jeu de données brut (car_clean) présente 539 véhicules, mettant en évidence d'importantes anomalies avant nettoyage. On y constate d'abord une forte présence de valeurs manquantes (NA), qui s'élèvent à environ 400 pour des critères précis comme la vitesse maximale (Top_Speed) ou le g.force, et à 37 pour la puissance (Horse_Power). De plus, les données brutes affichent des incohérences flagrantes, notamment des valeurs minimales à 0 pour la puissance et la vitesse de pointe, ainsi que des prix s'étalant de 0 à un maximum extrême de 50 millions de crédits, ce qui confirme la nécessité d'un traitement préalable.

# Suppression des colonnes avec plus de 28 % de NA
car_clean_2 <- car_clean %>%
  dplyr::select(-car_source_2, -X0.100_Mph, -Top_Speed, -X0.60_Mph, -g.force)

# Vérification après suppression
sort(colSums(is.na(car_clean_2)), decreasing = TRUE) #apres la suppression des grandes variables avec beaucoup de valeurs manquantes ils y'a des valeurs avec peu de variables qui nécessite le traitement et non la suppression des variables 

# Décomposition de Name_and_model en Year + Model
car_clean_2 <- car_clean_2 %>%
  mutate(
    Year  = as.character(str_extract(Name_and_model, "^\\d{4}")),
    Model = str_remove(Name_and_model, "^\\d{4}\\s+")
  ) %>%
  dplyr::select(-Name_and_model)

str(car_clean_2)

# Pourcentage de valeurs manquantes résiduelles
sort(colMeans(is.na(car_clean_2)) * 100, decreasing = TRUE)

# Vérification des doublons
sum(duplicated(car_clean_2))   # 0 doublons

# ==============================================================================
# 3A. ANALYSE DESCRIPTIVE — DONNÉES BRUTES (AVEC NA)
# Objectif : diagnostiquer les NA avant traitement
# ==============================================================================

summary(car_clean_2)

# Carte des valeurs manquantes
p_na <- car_clean_2 %>%
  is.na() %>%
  as.data.frame() %>%
  pivot_longer(cols = everything(),
               names_to = "Variable", values_to = "Manquant") %>%
  group_by(Variable) %>%
  summarise(nb_NA  = sum(Manquant),
            pct_NA = mean(Manquant) * 100,
            .groups = "drop") %>%
  filter(nb_NA > 0) %>%
  ggplot(aes(x = reorder(Variable, pct_NA), y = pct_NA, fill = pct_NA)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = paste0(round(pct_NA, 1), "% (n=", nb_NA, ")")),
            hjust = -0.1, size = 3.2) +
  coord_flip() +
  scale_fill_gradient(low = "#FFF9C4", high = "#E53935") +
  scale_y_continuous(limits = c(0, 90)) +
  labs(title    = "Variables avec valeurs manquantes — Données brutes",
       subtitle = "Rouge = suppression  |  Jaune = imputation par médiane",
       x = NULL, y = "% de NA") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))
print(p_na)

#Interpretation:
# L'analyse des valeurs manquantes sur les données brutes révèle deux variables principalement touchées : car_source_1 avec 11,5 % de NA (62 observations) et Horse_Power avec 6,9 % (37 observations). Les autres variables (Drive_Type, Model_type, Stock_Rating, speed, handling, braking, acceleration, launch, Offroad) présentent chacune un seul NA (0,2 %), un taux négligeable. Ce diagnostic guide les décisions de traitement qui seront appliquées dans l'étape suivante.

# Distribution de Horse_Power avant imputation (37 NA)
p_hp_brut <- ggplot(car_clean_2, aes(x = Horse_Power)) +
  geom_histogram(bins = 30, fill = "#EF9A9A", color = "white",
                 alpha = 0.8, na.rm = TRUE) +
  geom_vline(xintercept = median(car_clean_2$Horse_Power, na.rm = TRUE),
             color = "red", linetype = "dashed", linewidth = 1) +
  annotate("text",
           x     = median(car_clean_2$Horse_Power, na.rm = TRUE) + 100,
           y     = 25,
           label = paste0("Mediane = ",
                          round(median(car_clean_2$Horse_Power, na.rm = TRUE))),
           color = "red", size = 3.5) +
  labs(title    = "Distribution Horse_Power — données brutes (37 NA)",
       subtitle = "Ligne rouge = valeur d'imputation retenue",
       x = "Puissance (chevaux)", y = "Effectif") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))
print(p_hp_brut)

#Interpretation:
#La distribution est asymétrique à droite avec une majorité de véhicules entre 200 et 500 ch et une médiane de 375 ch. La médiane a été retenue comme valeur d'imputation car elle est robuste aux valeurs extrêmes des hypercars (jusqu'à 1000 ch) qui auraient biaisé la moyenne vers le haut.

# Scores de performance bruts
p_perf_brut <- car_clean_2 %>%
  dplyr::select(speed, handling, acceleration, launch, braking, Offroad) %>%
  pivot_longer(cols = everything(),
               names_to = "Variable", values_to = "Score") %>%
  ggplot(aes(x = Score, fill = Variable)) +
  geom_histogram(bins = 20, color = "white", alpha = 0.8,
                 show.legend = FALSE, na.rm = TRUE) +
  facet_wrap(~Variable, scales = "free_y") +
  labs(title    = "Scores de performance — données brutes (avec NA)",
       subtitle = "Exploratoire — NA ignorés par ggplot2",
       x = "Score (1-10)", y = "Effectif") +
  theme_minimal(base_size = 11) +
  theme(plot.title    = element_text(face = "bold"),
        plot.subtitle = element_text(color = "#E53935", size = 9))
print(p_perf_brut)

#Interpretation:
#Sur ce graphique, on remarque que l'accélération et la maniabilité ont des profils assez équilibrés qui se concentrent autour de la moyenne (entre 5 et 5,5), alors que le freinage et le départ sont globalement moins bons avec une majorité de notes situées vers le bas, autour de 3. Le critère tout-terrain est très particulier puisqu'il montre une uniformité totale avec un pic massif sur la note unique de 5, tandis que la vitesse se démarque nettement comme le point fort de l'échantillon avec des scores élevés qui culminent à 7,5.

# Distribution brute du prix
p_prix_brut <- ggplot(car_clean_2, aes(x = In_Game_Price)) +
  geom_histogram(bins = 40, fill = "#90A4AE", color = "white",
                 alpha = 0.8, na.rm = TRUE) +
  scale_x_continuous(labels = comma) +
  labs(title    = "Distribution du prix — données brutes (avec NA)",
       subtitle = "Confirme l'asymétrie à droite avant traitement",
       x = "Prix (crédits)", y = "Effectif") +
  theme_minimal(base_size = 11) +
  theme(plot.title    = element_text(face = "bold"),
        plot.subtitle = element_text(color = "#E53935", size = 9))
print(p_prix_brut)

#Interpretation:
#Sur ce graphique, on voit clairement que la distribution du prix est très asymétrique à droite, avec une immense majorité des données (plus de 450 individus) concentrée sur des prix très bas, proches de 0 crédit. En revanche, quelques rares valeurs isolées s'étalent très loin vers la droite, atteignant parfois plus de 15 millions, voire 50 millions de crédits, ce qui confirme la présence de fortes valeurs aberrantes ou d'une minorité d'éléments extrêmement chers avant traitement des données.

# ==============================================================================
# 3B. TRAITEMENT DES VALEURS MANQUANTES 
# ==============================================================================

# Suppression des colonnes inutiles ou trop lacunaires
car_clean_2 <- car_clean_2 %>%
  dplyr::select(-car_source_1)

if ("stock_specs" %in% colnames(car_clean_2)) {
  car_clean_2 <- car_clean_2 %>% dplyr::select(-stock_specs)
}
if ("Car_Image" %in% colnames(car_clean_2)) {
  car_clean_2 <- car_clean_2 %>% dplyr::select(-Car_Image)
}

# Suppression des lignes avec NA sur les variables qualitatives clés
car_clean_2 <- car_clean_2 %>%
  filter(!is.na(Model_type)) %>%
  filter(!is.na(Drive_Type))

# Imputation par la médiane pour les variables quantitatives
vars_quant_imp <- c("Horse_Power", "Stock_Rating", "speed", "handling",
                    "acceleration", "launch", "braking", "Offroad",
                    "In_Game_Price", "Weight_lbs")

car_clean_2 <- car_clean_2 %>%
  mutate(across(
    all_of(vars_quant_imp),
    ~ ifelse(is.na(.), median(., na.rm = TRUE), .)
  ))

# Vérification finale : doit afficher 0 partout
sort(colSums(is.na(car_clean_2)), decreasing = TRUE)
cat("Dimensions finales :", nrow(car_clean_2), "vehicules x",
    ncol(car_clean_2), "variables\n") # Apres nettoyage du jeu de donnée elle contient 537 lignes et 15 colonnes
cat("NA résiduels :", sum(is.na(car_clean_2)), "\n")
# On peut constater que le code affiche bien des zéros partout, ce qui prouve qu'il ne reste absolument aucune valeur manquante (NA résiduelle) dans l'ensemble des colonnes. Au final, après l'étape de traitement, notre jeu de données nettoyé (car_clean_2) possède des dimensions finales stables et prêtes pour l'analyse, comptant précisément 537 lignes (véhicules) et 15 colonnes (variables).

# ==============================================================================
# 3C. ANALYSE DESCRIPTIVE COMPLÈTE — DONNÉES NETTOYÉES (SANS NA)
# ==============================================================================

summary(car_clean_2)

#Interpretation: 
#le résumé statistique de la base nettoyée (car_clean_2), qui compte désormais 537 véhicules et ne présente plus aucune valeur manquante. On constate que les variables clés sont maintenant bien stabilisées : la puissance minimale (Horse_Power) a été corrigée et commence désormais à 4 chevaux pour un maximum de 986, tandis que la médiane des prix se fixe à 130 000 crédits. Concernant les notes de performance sur 10, la tendance se confirme avec des scores bien répartis, allant d'un minimum de 1.0 en accélération jusqu'à la note maximale de 10.0 atteinte sur l'ensemble des critères.

# Distribution des scores de performance
p_perf <- car_clean_2 %>%
  dplyr::select(speed, handling, acceleration, launch, braking, Offroad) %>%
  pivot_longer(cols = everything(),
               names_to = "Variable", values_to = "Score") %>%
  ggplot(aes(x = Score, fill = Variable)) +
  geom_histogram(bins = 20, color = "white", alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~Variable, scales = "free_y") +
  labs(title    = "Distribution des scores de performance — Données nettoyées",
       subtitle = "Echelle : 1 (mauvais) a 10 (excellent)",
       x = "Score", y = "Effectif") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))
print(p_perf)

#Interpretation:
#Sur ce graphique représentant les données nettoyées, on retrouve les mêmes distributions de performance que sur la base brute, ce qui prouve que la suppression des deux lignes aberrantes n'a pas modifié la tendance globale. L’accélération et la maniabilité restent centrées autour de 5, le freinage et le départ sont toujours dominés par des scores plus faibles (autour de 3), tandis que le critère tout-terrain conserve son pic massif d'uniformité à 5 et que la vitesse se confirme comme le point fort du lot avec une majorité de notes élevées culminant à 7,5.

# Distribution du prix
p_prix <- ggplot(car_clean_2, aes(x = In_Game_Price)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  coord_cartesian(xlim = c(0, 5000000)) +
  scale_x_continuous(labels = comma) +
  labs(title    = "Distribution des prix en jeu",
       subtitle = "Distribution asymétrique à droite",
       x = "Prix (crédits)", y = "Effectif") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))
print(p_prix)

#Interpretation:
#Sur ce graphique représentant les données nettoyées, on constate que la distribution des prix reste fortement asymétrique à droite, mais l'échelle de l'axe des abscisses est désormais beaucoup plus lisible, car elle s'arrête autour de 5 millions de crédits au lieu de 50 millions. Cela confirme que le nettoyage a permis de traiter ou d'isoler les valeurs aberrantes extrêmes, même si la grande majorité des véhicules (plus de 450) reste concentrée sur des tarifs très accessibles en dessous du million de crédits.

# Horse_Power après imputation
p_hp_clean <- ggplot(car_clean_2, aes(x = Horse_Power)) +
  geom_histogram(bins = 30, fill = "#4CAF50", color = "white", alpha = 0.8) +
  geom_vline(xintercept = median(car_clean_2$Horse_Power),
             color = "red", linetype = "dashed", linewidth = 1) +
  annotate("text",
           x     = median(car_clean_2$Horse_Power) + 100,
           y     = 25,
           label = paste0("Mediane = ", round(median(car_clean_2$Horse_Power))),
           color = "red", size = 3.5) +
  labs(title    = "Distribution Horse_Power — après imputation",
       subtitle = "37 NA remplacés par la médiane",
       x = "Puissance (chevaux)", y = "Effectif") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))
print(p_hp_clean)

#Interpretation:
#Ce graphique montre la distribution de la puissance en chevaux (Horse_Power) après l'étape d'imputation, où les 37 valeurs manquantes (NA) ont été remplacées par la valeur médiane de 375 chevaux. Ce choix méthodologique se traduit visuellement par un pic très net et artificiel d'effectifs (marqué par la ligne pointillée rouge) à ce niveau précis, tandis que le reste du graphique conserve une répartition étalée allant de près de 0 à 1000 chevaux.

# ==============================================================================
# 4. ACP — ANALYSE EN COMPOSANTES PRINCIPALES
# ==============================================================================

# --- 4.1 Sélection des variables quantitatives actives ---
data_quant <- car_clean_2 %>%
  select_if(is.numeric)

# --- 4.2 Imputation des NA résiduels par la médiane (sécurité avant PCA) ---
data_quant <- data_quant %>%
  mutate(across(everything(), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))

colSums(is.na(data_quant))   # doit afficher 0 partout

# Résumé des variables actives
summary(data_quant)
#Interpretation:
#le résumé statistique obtenu avec la commande summary(data_quant), qui isole uniquement nos 10 variables quantitatives actives pour la suite des analyses (comme une ACP). On y retrouve les indicateurs financiers avec le prix (In_Game_Price), l'évaluation globale (Stock_Rating), les caractéristiques physiques (le poids et la puissance en chevaux) ainsi que les 6 scores de performance sur 10. Les statistiques confirment que les données sont désormais parfaitement nettoyées, sans aucune valeur manquante, avec des valeurs cohérentes prêtes à être exploitées.

# --- 4.3 Nuage de points multivariés ---
pairs(data_quant, col = "steelblue", pch = 16, cex = 0.4,
      main = "Nuage de points multivariés — variables quantitatives")
#Interpretation:
#Cette matrice de nuages de points montre les relations deux à deux entre toutes nos variables quantitatives, révélant de fortes corrélations linéaires, notamment entre la vitesse, l'accélération, la puissance et l'évaluation globale (Stock_Rating). À l'inverse, d'autres caractéristiques comme le tout-terrain (Offroad) ou le poids affichent des nuages beaucoup plus diffus. Cette forte interdépendance et la redondance d'information entre plusieurs variables prouvent la pertinence d'utiliser une Analyse en Composantes Principales (ACP), qui permettra de résumer efficacement ce jeu de données en réduisant le nombre de dimensions sur quelques axes clés.

# --- 4.4 Calcul de l'ACP ---
# Assignation des noms de modèles comme rownames
rownames(data_quant) <- make.unique(car_clean_2$Model)

res.pca <- PCA(data_quant, scale.unit = TRUE, graph = FALSE)
summary(res.pca)

# --- 4.5 Valeurs propres et choix du nombre d'axes ---
get_eigenvalue(res.pca)
round(res.pca$eig, 3)

# Éboulis — règle de Kaiser (λ > 1)
p_eig1 <- fviz_screeplot(res.pca, addlabels = TRUE, choice = "eigenvalue",
                         ggtheme = theme_minimal(),
                         main    = "Valeurs propres — seuil Kaiser (lambda > 1)",
                         ylab    = "Valeur propre") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red", linewidth = 0.8)
print(p_eig1)
#Interpretation:
#le tableau des valeurs propres (eigenvalues) issues de notre ACP, qui permet de choisir le nombre d'axes à retenir pour l'analyse. En appliquant la règle de Kaiser (retenir les axes associés à une valeur propre supérieure à 1), on constate que les trois premières composantes principales doivent être conservées, avec des valeurs propres respectives de 5,55, 1,46 et 1,02. Ensemble, ces trois premiers axes capturent une variance cumulée de 80,19% (soit 55,45% pour l'axe 1, 14,59% pour l'axe 2 et 10,15% pour l'axe 3), ce qui garantit une excellente restitution de l'information globale contenue dans le jeu de données initial.

# Variance expliquée par axe
p_eig2 <- fviz_screeplot(res.pca, addlabels = TRUE, choice = "variance",
                         ggtheme = theme_minimal(),
                         main    = "Variance expliquée par axe (%)")
print(p_eig2)
#Interpretation:
#Ce graphique en barres (appelé scree plot ou éboulis des valeurs propres) illustre le pourcentage de variance expliquée par chacune des dix dimensions de l'ACP. On observe une cassure très nette après le premier axe, qui capte à lui seul 55,5% de l'inertie totale, suivi des axes 2 et 3 qui restituent respectivement 14,6% et 10,2% de l'information. Le graphique montre une décroissance plus douce et linéaire à partir de la quatrième dimension, ce qui confirme visuellement le choix de retenir les trois premiers axes pour l'analyse, puisqu'ils permettent de conserver plus de 80% de la variance globale du jeu de données.

# --- 4.6 Cercle des corrélations ---
round(res.pca$var$cor, 2)

# Plan Dim.1 x Dim.2
par(mfrow = c(1, 2))
plot(res.pca, choix = "var")
#Interpretation:
#Ce cercle des corrélations de l'ACP (axes 1 et 2) permet d'interpréter facilement les relations entre les variables :
#L'axe 1 (55,45 % d'inertie) représente la performance globale sur route : Presque toutes les variables de performance (speed, braking, handling, acceleration, launch) ainsi que la puissance (Horse_Power) et la note globale (Stock_Rating) pointent très fortement vers la droite. Elles sont très corrélées entre elles : un véhicule rapide est aussi puissant, freine bien et se manie bien.
#L'axe 2 (14,59 % d'inertie) oppose le profil des véhicules : Il oppose le poids (Weight_lbs) tout en haut au prix en jeu (In_Game_Price) plutôt vers le bas.
#Le cas de l'Offroad : La flèche pointe vers le haut à gauche, ce qui montre que les véhicules à l'aise en tout-terrain (Offroad) sont plutôt lourds (du même côté sur l'axe 2), mais ont tendance à avoir de moins bons scores de vitesse ou d'accélération sur route (opposés sur l'axe 1).

plot(res.pca, axes = c(1, 3), choix = "var")
par(mfrow = c(1, 1))
#Interpretation:
#Ce second cercle des corrélations de l'ACP (axes 1 et 3) permet de compléter l'analyse en mettant en lumière le rôle du troisième axe factoriel :
#L'axe 1 (55,45 % d'inertie) : Conserve la même signification, en regroupant fortement vers la droite les performances sur route (speed, braking, handling, etc.) à l'opposé des capacités Offroad situées à gauche.
#L'axe 3 (10,15 % d'inertie) est l'axe du prix : Il est presque exclusivement défini par la variable In_Game_Price, qui pointe verticalement tout en haut du cercle.
#Cette projection montre que le prix en jeu est une dimension totalement indépendante des performances pures (les flèches de performance forment un angle droit avec celle du prix). Cela signifie que le coût d'un véhicule n'est pas forcément lié à sa vitesse ou à sa maniabilité, ce qui apporte une information inédite que les deux premiers axes n'exprimaient pas.

##A quoi correspond les axes 1 , 2, 3 savoir ce qu'elle correspond 

# --- 4.7 Contributions et cos2 des variables ---
round(res.pca$var$cos2,    3)
round(res.pca$var$contrib, 3)
#Interpretation:
#la validation chiffrée de notre ACP à travers la qualité de représentation ($\cos^2$) et les contributions (en %) des variables :La dimension 1 est bien l'axe de la performance globale : elle est portée de manière équitable par les notes sur route (Stock_Rating, braking, handling, speed, acceleration) qui affichent de très forts $\cos^2$ (supérieurs à 0,75) et des contributions individuelles entre 11 % et 16 %.La dimension 2 est définie par le profil physique et l'usage : le poids (Weight_lbs) et le tout-terrain (Offroad) la dominent largement, représentant à eux deux près de 78 % de sa contribution totale (44,4 % et 33,3 %).La dimension 3 est purement financière : le prix (In_Game_Price) s'y isole de façon spectaculaire avec un $\cos^2$ de 0,747 et une contribution exclusive de 73,6 %, ce qui confirme mathématiquement que le coût d'un véhicule est indépendant de ses performances.

# Contributions à Dim.1
p_contrib1 <- fviz_contrib(res.pca, choice = "var", axes = 1, top = 10,
                           ggtheme = theme_minimal(),
                           title   = "Contribution des variables à Dim.1") +
  geom_hline(yintercept = 10, linetype = "dashed", color = "red") +
  annotate("text", x = 1, y = 10.5, label = "Seuil (10 %)",
           color = "red", size = 3, hjust = 0)
print(p_contrib1)
#Interpretation:
#Ce graphique en barres montre précisément la contribution de chaque variable à la construction du premier axe de l'ACP (Dim.1). La ligne pointillée rouge matérialise le seuil théorique de contribution moyenne, fixé ici à 10 %.
#On constate de manière très nette que sept variables dépassent ce seuil et portent l'essentiel de l'information de cet axe : l'évaluation globale (Stock_Rating), le freinage (braking), la maniabilité (handling), l'accélération (acceleration), la vitesse (speed), le départ (launch) et la puissance (Horse_Power). Leurs contributions respectives, comprises entre 11 % et 16 %, confirment graphiquement que le premier axe factoriel correspond strictement à la performance globale sur route des véhicules, tandis que les capacités tout-terrain (Offroad), le prix (In_Game_Price) et le poids (Weight_lbs) y jouent un rôle négligeable.

# Contributions à Dim.2
p_contrib2 <- fviz_contrib(res.pca, choice = "var", axes = 2, top = 10,
                           ggtheme = theme_minimal(),
                           title   = "Contribution des variables à Dim.2") +
  geom_hline(yintercept = 10, linetype = "dashed", color = "red") +
  annotate("text", x = 1, y = 10.5, label = "Seuil (10 %)",
           color = "red", size = 3, hjust = 0)
print(p_contrib2)
#Interpretation:
#Ce graphique en barres montre la contribution de chaque variable à la construction de la deuxième dimension de l'ACP (Dim.2), avec la ligne rouge en pointillé indiquant le seuil théorique de contribution moyenne à 10 %.
#On voit tout de suite que cet axe est ultra-dominé par deux variables seulement qui dépassent largement ce seuil : le poids (Weight_lbs), qui contribue à hauteur d'environ 44 %, et les capacités tout-terrain (Offroad), à environ 33 %. À elles deux, elles représentent plus des trois quarts de l'information de l'axe, confirmant graphiquement que la Dim.2 définit principalement le gabarit physique et l'usage des véhicules (les modèles lourds adaptés au tout-terrain), tandis que le prix et les performances sur route pure y jouent un rôle très secondaire.

# Contributions à Dim.3
p_contrib3 <- fviz_contrib(res.pca, choice = "var", axes = 3, top = 10,
                           ggtheme = theme_minimal(),
                           title   = "Contribution des variables à Dim.3") +
  geom_hline(yintercept = 10, linetype = "dashed", color = "red") +
  annotate("text", x = 1, y = 10.5, label = "Seuil (10 %)",
           color = "red", size = 3, hjust = 0)
print(p_contrib3)
#Interpretation:
#Ce graphique en barres montre la contribution des variables à la troisième dimension de l'ACP (Dim.3), avec la ligne rouge en pointillé indiquant le seuil théorique de 10 %.
#On constate une hégémonie absolue d'une seule variable : le prix en jeu (In_Game_Price), qui culmine de façon spectaculaire à près de 74 % de contribution. Le poids (Weight_lbs) atteint tout juste le seuil des 10 %, tandis que toutes les autres caractéristiques techniques et de performance ont un impact insignifiant. Cela confirme graphiquement que la Dim.3 est un axe purement financier, validant le fait que le prix des véhicules évolue de façon totalement indépendante de leurs performances sur route ou tout-terrain.

# --- 4.8 Graphique des individus ---
round(res.pca$ind$cos2,    3)
round(res.pca$ind$contrib, 3)
#Interpretation:

# Plan Dim.1 x Dim.2 et Dim.1 x Dim.3
# --- Graphique des individus lisible ----------------------------------------

fviz_pca_ind(
  res.pca,
  
  axes = c(1, 2),
  
  repel = TRUE,            # évite le chevauchement des labels
  
  col.ind = "cos2",        # couleur selon qualité de représentation
  
  gradient.cols = c("#BBDEFB", "#1565C0", "#0D47A1"),
  
  alpha.ind = 0.7,
  
  select.ind = list(
    cos2 = 20              # garde seulement les 20 mieux représentés
  ),
  
  title = "ACP — individus les mieux représentés"
)

#interpretation:
#Ce graphique des individus (axes 1 et 2) classe les véhicules les mieux représentés ($\cos^2$ élevé) en quatre profils distincts :Les supercars (En haut à droite) : Lamborghini Aventador, Audi R8, Porsche Carrera GT. Elles combinent une puissance et des performances sur route maximales (Dim.1 positive, Dim.2 positive).Les purs Tout-Terrain (En haut à gauche) : Jeep Gladiator et Wrangler. Portées par leur poids et leur note Offroad, elles affichent cependant de faibles performances sur route (Dim.2 positive, Dim.1 négative).Les sportives historiques légères (En bas à droite) : Saleen S7, Toyota 2000GT. Très performantes sur asphalte, mais avec un profil bas et léger à l'opposé des SUV (Dim.1 positive, Dim.2 négative).Les citadines et anciennes classiques (En bas à gauche) : Volkswagen Golf GTi Mk2, Bugatti Type 35 C, Chevrolet Bel Air. Des modèles légers, non adaptés au tout-terrain et aux performances modestes sur route (Dim.1 et Dim.2 négatives)

# --- Graphique des individus lisible ----------------------------------------

fviz_pca_ind(
  res.pca,
  
  axes = c(1, 3),
  
  repel = TRUE,            # évite le chevauchement des labels
  
  col.ind = "cos2",        # couleur selon qualité de représentation
  
  gradient.cols = c("#BBDEFB", "#1565C0", "#0D47A1"),
  
  alpha.ind = 0.7,
  
  select.ind = list(
    cos2 = 20              # garde seulement les 20 mieux représentés
  ),
  
  title = "ACP — individus les mieux représentés"
)

#Interpretation:
#Ce graphique des individus (axes 1 et 3) met en lumière la dimension financière du jeu de données :
#L'élite des collectionneurs (Tout en haut) : La Ferrari 250 GTO s'isole au sommet absolu de l'axe 3, suivie par la Ford GT40 Mk II et la Ferrari 250 Testa Rossa. Cela confirme que cet axe vertical est uniquement dicté par le prix astronomique et la rareté de ces modèles historiques.
#Déconnexion Prix / Performance : Ces voitures de collection se dispersent horizontalement sur l'axe 1. La Ferrari 250 GTO affiche des performances moyennes (au centre), tandis que la GT40 est ultra-performante (à droite), prouvant qu'un prix record ne garantit pas la meilleure efficacité sur piste.
#Le marché grand public (Tout en bas) : La quasi-totalité des autres véhicules (ex: Toyota 2000GT, Volkswagen Golf) reste écrasée au bas du graphique, signe d'un coût standard et accessible, indépendamment de leurs caractéristiques techniques.

# --- 4.9 Projection de la variable qualitative Model_type ---
# Regroupement des 38 catégories en 7 familles cohérentes
car_clean_2 <- car_clean_2 %>%
  mutate(Model_groupe = case_when(
    Model_type %in% c("HYPERCARS", "MODERN SUPERCARS", "RETRO SUPERCARS",
                      "SUPER GT", "GT CARS")                              ~ "Supercars/GT",
    Model_type %in% c("MODERN SPORTS CARS", "RETRO SPORTS CARS",
                      "CLASSIC SPORTS CARS", "SPORTS UTILITY HEROES",
                      "HOT HATCH", "SUPER HOT HATCH",
                      "RETRO HOT HATCH")                                  ~ "Sports/Hot Hatch",
    Model_type %in% c("CLASSIC MUSCLE", "MODERN MUSCLE", "RETRO MUSCLE",
                      "DRIFT CARS", "TRACK TOYS",
                      "EXTREME TRACK TOYS")                               ~ "Muscle/Track",
    Model_type %in% c("CLASSIC RACERS", "CLASSIC RALLY", "MODERN RALLY",
                      "RALLY MONSTERS", "RETRO RALLY")                    ~ "Rally/Race",
    Model_type %in% c("OFFROAD", "PICK-UP & 4X4'S", "TRUCKS",
                      "UNLIMITED OFFROAD", "UNLIMITED BUGGIES",
                      "BUGGIES", "UTV'S")                                 ~ "Tout-terrain",
    Model_type %in% c("CULT CARS", "CULT CLASSICS", "RARE CLASSICS",
                      "VINTAGE RACERS", "RODS AND CUSTOMS",
                      "CLASSIC CARS")                                     ~ "Classiques/Retro",
    Model_type %in% c("RETRO SALOONS", "SUPER SALOONS",
                      "VANS AND UTILITY")                                 ~ "Utilitaires/Saloons",
    TRUE ~ "Autres"
  ))

model_groupe_factor <- as.factor(car_clean_2$Model_groupe)

# Individus colorés par famille (Model_groupe)
p_model_acp <- fviz_pca_ind(res.pca,
                            habillage    = model_groupe_factor,
                            addEllipses  = FALSE,
                            geom.ind     = "point",
                            pointsize    = 2,
                            repel        = FALSE,
                            palette      = c("#E53935", "#1E88E5", "#43A047",
                                             "#FB8C00", "#8E24AA", "#00ACC1",
                                             "#795548", "#F06292"),
                            ggtheme      = theme_minimal(),
                            title        = "ACP — Individus colorés par famille de véhicules",
                            legend.title = "Famille") +
  guides(shape = "none") +
  theme(legend.position = "right",
        legend.text      = element_text(size = 9))
print(p_model_acp)
#Interpretation:
#Ce graphique de l'ACP (axes 1 et 2) segmente visuellement le catalogue selon trois grands profils de familles :
#Le pôle Tout-terrain (Haut gauche) : Les triangles bleus clairs s'isolent nettement au sommet vertical, confirmant une forte note en Offroad (Dim.2 positive) associée à des performances limitées sur circuit (Dim.1 négative).
#Le pôle Supercars/GT et Muscle (Droite) : Les losanges violets et triangles bleus foncés se regroupent à l'extrême droite, caractérisant les véhicules aux performances et à la puissance maximales sur asphalte (Dim.1 positive).
#Le pôle Classiques et Sports/Hot Hatch (Bas gauche) : Les cercles rouges et croix orange s'entassent dans le quadrant inférieur gauche, regroupant les voitures légères, peu adaptées aux pistes accidentées et aux performances modestes face aux sportives modernes (Dim.1 et Dim.2 négatives).

# Individus colorés par transmission (Drive_Type)
drive_type_factor <- as.factor(car_clean_2$Drive_Type)

p_drive_acp <- fviz_pca_ind(
  res.pca,
  habillage = drive_type_factor,
  geom.ind  = "text",
  col.ind   = drive_type_factor,
  
  # garde seulement les 25 meilleurs individus
  select.ind = list(contrib = 25),
  
  repel = TRUE,
  palette = c("#E53935", "#1E88E5", "#43A047"),
  
  ggtheme = theme_minimal(),
  title = "ACP — Individus les plus contributifs",
  legend.title = "Transmission"
)

print(p_drive_acp)
#Interpretation:
#Ce graphique de l'ACP (axes 1 et 2) regroupe les véhicules qui ont le plus de poids (contributions) dans la création des axes :
#L'élite de la performance (À droite) : Dominé par des hypercars à transmission intégrale (AWD - en rouge) comme la Porsche 918 Spyder et la Lamborghini Sesto Elemento. Elles définissent le pôle de la vitesse maximale.
#Les colosses du Tout-Terrain (En haut à gauche) : Marqués par des monstres physiques comme le HUMMER H1 Alpha et le Mercedes 6x6. Ils tirent l'axe 2 vers le haut par leur poids et leur profil Offroad.
#Les micro-voitures (En bas à gauche) : Les modèles les plus légers et moins puissants du jeu (Peel P50, BMW Isetta, Renault 4L) s'opposent point par point aux deux catégories précédentes (performances et poids minimaux).

# --- 4.10 Biplot ---

#Le biplot combine sur le même graphique :

#  * Les individus (points)
#* Les variables (flèches)

#Règle empirique :

#  Si les deux premières composantes principales expliquent plus de 70 % de l’inertie totale, alors le plan factoriel (Dim 1, Dim 2) capture la majorité de l’information et le biplot est fiable pour interpréter simultanément individus et variables.

#Logique :

#  Si Dim 1 + Dim 2 < 50 %
#→ beaucoup d’information reste sur les axes supérieurs
#→ biplot en 2D serait trompeur, on perdrait l’essentiel

#Si Dim 1 + Dim 2 > 70 %
#→ plan 2D représente plus des 2/3 de l’information
#→ les distances et angles sont fiables pour l’interprétati

#Dans mon cas :

# Dim.1 + Dim.2 = 69.9 % ~ 70 % -> biplot fiable
# Seuls les 20 individus les mieux représentés (cos2) sont affichés
p_biplot <- fviz_pca_biplot(res.pca,
                            repel      = TRUE,
                            col.var    = "#E53935",
                            col.ind    = "#90A4AE",
                            alpha.ind  = 0.6,
                            geom.ind   = c("point", "text"),
                            geom.var   = c("arrow", "text"),
                            pointsize  = 1.5,
                            labelsize  = 3,
                            select.ind = list(cos2 = 20),
                            ggtheme    = theme_minimal(),
                            title      = "Biplot ACP — Forza Horizon (Dim.1 x Dim.2)",
                            subtitle   = paste0("Variance expliquée : ",
                                                round(sum(res.pca$eig[1:2, 2]), 1), " %"))
print(p_biplot)

#Interpretation:
#Ce biplot ACP (axes 1 et 2) résume l'analyse en superposant directement les variables (flèches rouges) et les véhicules phares (en bleu) :
#Les supercars performantes (À droite) : Portées par toutes les flèches de performance routière, de puissance et de vitesse, on y retrouve logiquement les Lamborghini Aventador, Audi R8 et Porsche Carrera GT.
#Les baroudeurs lourds (En haut à gauche) : Les Jeep Gladiator et Wrangler s'alignent parfaitement sur les flèches du poids (Weight_lbs) et des capacités Offroad.
#Les modèles modestes et légers (En bas à gauche) : À l'exact opposé des flèches de performance et de poids, ce secteur regroupe les citadines et anciennes classiques (Volkswagen Golf, Bugatti Type 35 C, Chevrolet Bel Air).
#L'axe financier (Vers le bas) : La flèche In_Game_Price pointe vers le bas, isolant les modèles aux tarifs plus élevés sur ce plan.

#Conclusiopn par rapport a l'ACP selon la con,texte

# ==============================================================================
# 5. AFC — ANALYSE FACTORIELLE DES CORRESPONDANCES
# ==============================================================================

# --- 5.1 Tableau de contingence Model_type x car_source ---
# Variables retenues : Model_type (catégorie du véhicule) et car_source (mode d'obtention)
# Ces deux variables sont liées : certaines catégories sont associées
# à des modes d'obtention spécifiques (ex : hypercars via Wheelspin ou DLC)

car_afc <- car_clean_2 %>%
  filter(!is.na(Model_type) & !is.na(car_source))

tab_contingence <- table(car_afc$Model_type, car_afc$car_source)
tab_contingence

# --- VISUALISATION DES PROFILS ---
# Abréviation des noms de catégories pour lisibilité
rownames(tab_contingence) <- abbreviate(rownames(tab_contingence), 
                                        minlength = 5, 
                                        use.classes = TRUE)

# Visualisation des profils lignes et colonnes
p_profils <- as.data.frame(prop.table(tab_contingence, margin = 1)) %>%
  setNames(c("Model_type", "car_source", "Proportion")) %>%
  ggplot(aes(x = car_source, y = Model_type, fill = Proportion)) +
  geom_tile(color = "white") +
  geom_text(aes(label = ifelse(Proportion > 0.05, 
                               paste0(round(Proportion * 100, 1), "%"), "")),
            size = 2.5, color = "white") +
  scale_fill_gradient(low = "#E3F2FD", high = "#1565C0",
                      labels = percent) +
  labs(title    = "Profils lignes — Model_type x car_source",
       subtitle = "Proportion de chaque source au sein de chaque catégorie",
       x = "Mode d'obtention", y = "Catégorie",
       fill = "Proportion") +
  theme_minimal(base_size = 10) +
  theme(axis.text.x  = element_text(angle = 30, hjust = 1, size = 8),
        axis.text.y  = element_text(size = 7),
        plot.title   = element_text(face = "bold"))
print(p_profils)
#Interpretation:
#Ce tableau thermique (heatmap) croise les catégories de véhicules (Model_type) avec leur mode d'obtention dans le jeu (car_source), préparant l'Analyse des Correspondances Multiples (ACM) ou une AFC :
#La source universelle (Autoshow) : La colonne Autoshow se détache nettement par sa couleur bleue sur presque toutes les lignes. Cela montre que l'achat direct au salon de l'auto reste le mode d'obtention ultra-majoritaire et transversal pour quasiment toutes les catégories de voitures.
#Les modes d'obtention exclusifs ou rares : Les autres colonnes (Barn pour les trésors de grange, Wheelspin, Season Event) restent très claires (proches de 0 %). Elles représentent des canaux d'acquisition secondaires ou restreints à des types de véhicules très précis.
#Pertinence pour l'analyse : Cette forte concentration des données sur la source Autoshow crée un déséquilibre. Dans la suite de l'analyse, il faudra surveiller si cette omniprésence n'écrase pas les autres modalités ou si des catégories rares s'associent spécifiquement aux sources secondaires.

# --- 5.2 Test du Chi-deux ---

chi2 <- chisq.test(tab_contingence)
chi2
# H0 : Model_type et car_source sont indépendantes
# La p-value obtenue (0.09858) est supérieure au seuil de 5 %.
# On ne rejette donc pas l’hypothèse d’indépendance.
# Aucun lien statistiquement significatif n’est mis en évidence entre
# le type de véhicule et le mode d’obtention.
# L’AFC reste toutefois pertinente dans une logique exploratoire
# afin d’identifier d’éventuelles tendances structurelles.

# Contributions locales au Chi2
round(chi2$residuals^2, 2)

# Calcule l'inertie totale du tableau (mesure globale de la dispersion/différenciation)
chi2$statistic/(nrow(tab_contingence)*ncol(tab_contingence)) 

# Calcule l'inertie par ligne et trie les CSP les plus "typiques" dans leur consommation média
chi2$statistic/nrow(tab_contingence)  ; sort(apply(chi2$residuals^2,1,"sum"), decreasing = T) # contribution pour les lignes 

# Calcule l'inertie par colonne et trie les médias qui discriminent le plus les CSP
chi2$statistic/ncol(tab_contingence)  ; sort(apply(chi2$residuals^2,2,"sum"), decreasing = T) # contribution pour les colonnes

# Catégories les plus contributives au Chi2
sort(apply(chi2$residuals^2, 1, sum), decreasing = TRUE)
sort(apply(chi2$residuals^2, 2, sum), decreasing = TRUE)

# --- 5.3 Profils lignes et colonnes ---
# Profils lignes : répartition des sources par catégorie
round(prop.table(tab_contingence, margin = 1), 3)

# Profils colonnes : répartition des catégories par source
round(prop.table(tab_contingence, margin = 2), 3)

# Marges
round(margin.table(tab_contingence, 2) / sum(tab_contingence), 3)
round(margin.table(tab_contingence, 1) / sum(tab_contingence), 3)

# --- 5.4 Calcul de l'AFC ---
res2.ca <- CA(tab_contingence, graph = FALSE)
summary(res2.ca)

# --- 5.5 Valeurs propres ---
res2.ca$eig ; fviz_eig(res2.ca)
#NB:Bien que la cassure visuelle du graphique se situe après le 5e axe, il y a une règle d'or très stricte en Analyse des Correspondances (AFC / ACM) : on ne garde que les axes qui apportent une vraie valeur explicative et qui sont facilement interprétables.
#Interpretation : 
#Voici l'analyse essentielle des valeurs propres de votre Analyse des Correspondances (AFC) :
#Choix du nombre d'axes (Règle du coude) : Le scree plot et le tableau montrent une décroissance régulière de la variance expliquée sur les cinq premières dimensions, suivie d'une cassure très nette (un "coude") après la dimension 5. On retient donc les 5 premiers axes pour l'analyse.
#Variance cumulée : Ces 5 premiers axes restituent à eux seuls 89,73 % de l'inertie totale du tableau de contingence (soit 26,11 % pour l'axe 1, 22,35 % pour l'axe 2, 16,91 % pour l'axe 3, 13,34 % pour l'axe 4 et 11,02 % pour l'axe 5). C'est un excellent score qui garantit une perte d'information quasi nulle.
#Spécificité de l'AFC : Les valeurs propres (eigenvalues) globales sont faibles (maximum 0,16), ce qui est tout à fait normal et classique en AFC, car elles dépendent de l'inertie totale du tableau croisé et non du nombre de variables comme en ACP.

# --- ANALYSE DE L'AXE 1 (Dimension 1) ---
# Trie les model_type qui contribuent le plus à la construction de l'axe 1
round(sort(res2.ca$row$contrib[,1], decreasing=T), 3) 
fviz_contrib(res2.ca, choice = "row", axe=1)
#Interpretation:
#Ce graphique montre la contribution des lignes (les catégories de véhicules) à la construction de la première dimension (Dim-1) de l'AFC :
#L'hyper-domination des Cult Classics : La catégorie CULTCL (Cult Classics) écrase complètement cet axe avec une contribution majeure de près de 45 % à elle seule.
#Les autres contributeurs clés : Les catégories HYPER (Hypercars), CLASM (Classic Muscle), VINTR (Vintage Rally) et CULTCA (Cult Cars) franchissent également le seuil moyen (ligne rouge) avec des contributions allant de 5 % à 12 %.
#Interprétation de Dim-1 : Puisque nous savons que l'axe 1 des colonnes était défini par Season Event et Barn, ce graphique montre la correspondance parfaite avec les lignes. La Dimension 1 oppose les catégories de voitures "récompenses" ou "rares" (comme les HYPER ou les voitures de collection trouvées dans les granges) aux voitures plus populaires et standards (CULTCL), reflétant fidèlement le clivage sur l'exclusivité des véhicules.

# Trie les car_source  qui contribuent le plus à la construction de l'axe 1
round(sort(res2.ca$col$contrib[,1], decreasing=T), 3) 
fviz_contrib(res2.ca, choice = "col", axe=1)
#Interpretation:
#Ce graphique montre la contribution des colonnes (les modes d'obtention) à la construction de la première dimension (Dim-1) de l'AFC :
#L'hégémonie de Season Event : La modalité Season Event (Événements de saison) écrase totalement cet axe avec une contribution spectaculaire de près de 80 %. Elle dépasse massivement la ligne rouge du seuil théorique moyen (fixé ici à 12,5 %).
#Le rôle secondaire de Barn : Seule la modalité Barn (Trésors de grange) franchit également le seuil de justesse, avec un peu plus de 12 % de contribution.
#Interprétation de Dim-1 : La Dimension 1 est un axe d'exclusivité temporelle. Elle isole complètement les véhicules qui s'obtiennent via des récompenses limitées dans le temps (Season Event) ou des événements uniques (Barn), par opposition au reste des modes d'obtention classiques du jeu (comme l'achat direct au salon qui a une contribution proche de zéro sur cet axe).

# ANALYSE DE L'AXE 2 (Dimension 2)
# Même chose pour le deuxième axe (différences secondaires)
round(sort(res2.ca$row$contrib[,2], decreasing=T), 3) 
fviz_contrib(res2.ca, choice = "row", axe=2)
# Interprete:
#le graphique montre la contribution des lignes (les catégories de véhicules) à la construction de la deuxième dimension (Dim-2) de l'AFC :
#La domination des Tracteurs/Camions : La modalité TRACT (qui correspond aux camions de course ou tracteurs/gros utilitaires) domine de façon écrasante cet axe avec près de 40 % de contribution à elle seule, franchissant largement le seuil moyen (ligne rouge).
#Les autres catégories significatives : Les catégories RALLM (Rally Monsters), CULTCA (Cult Cars) et EXTTT (Extreme Off-Road) dépassent également le seuil, avec des contributions comprises entre 5 % et 10 %.
#Interprétation de Dim-2 : La Dimension 2 est l'axe des véhicules atypiques et extrêmes. Elle isole les catégories très spécifiques du jeu (TRACT, RALLM, EXTTT) qui s'écartent des voitures de tourisme ou de sport standards et qui possèdent généralement des modes d'obtention bien particuliers.

round(sort(res2.ca$col$contrib[,2], decreasing=T), 3) 
fviz_contrib(res2.ca, choice = "col", axe=2) 
# Interpretation :
#Voici la synthèse essentielle de votre Analyse des Correspondances (AFC) :
#La dimension 1 sépare l'exclusivité et la rareté : elle oppose radicalement la catégorie CULTCL (Cult Classics) aux modes d'obtention exclusifs Season Event et Barn. Cet axe sépare les véhicules populaires accessibles des modèles rares à durée limitée.
#La dimension 2 isole le profil atypique : elle est dominée par les catégories extrêmes comme TRACT (Tracteurs/Camions), portées par les modes d'obtention liés à la progression et à la chance (Wheelspin et Accolade). Cet axe capte les véhicules bonus offerts en récompense de jeu.

# GRAPHIQUES ET QUALITÉ DE REPRÉSENTATION (cos2)

# Affiche le plan factoriel avec uniquement les points très bien représentés (cos2 > 0.8 ou 0.7)

#Générer le Biplot AFC interactif et propre (corrigé avec le code HEX pour crimson)
# 1. Charger le package indispensable pour nettoyer le graphique
library(factoextra)

# 2. Générer le graphique avec le texte des individus bien visible
fviz_ca_biplot(res2.ca, 
               select.row = list(cos2 = 0.7),  # Filtre les lignes (individus/catégories)
               select.col = list(cos2 = 0.7),  # Filtre les colonnes (modes d'obtention)
               geom.row = c("point", "text"),  # FORCE l'affichage des points ET du texte pour les lignes
               geom.col = c("point", "text"),  # FORCE l'affichage des points ET du texte pour les colonnes
               repel = TRUE,                   # Empêche les textes de se chevaucher
               col.row = "royalblue", 
               col.col = "#DC143C",
               ggtheme = theme_minimal()) +
  labs(title = "Cartographie AFC — Profils filtrés (cos² > 0.8)",
       x = "Dim 1 (26.11%)", 
       y = "Dim 2 (22.35%)")
#Interpretation:
#L’analyse de cette cartographie AFC montre que la distribution du catalogue s'articule autour d'une masse centrale standardisée et de deux trajectoires d'acquisition hautement spécifiques. D'une part, la Dimension 1 met en évidence une forte liaison exclusive entre la catégorie des véhicules populaires CULTCL (Cult Classics) et le mode d'obtention Season Event, prouvant que l'accès à ces modèles dépend de manière critique des récompenses à durée limitée. D'autre part, la Dimension 2 isole verticalement les engins lourds et atypiques comme les camions (TRACT), indiquant qu'ils obéissent à un parcours d'acquisition distinct des circuits commerciaux classiques, tandis que le reste des catégories majeures s'agglomère près de l'origine autour de l'acheteur universel Autoshow.

# Générer le Biplot AFC interactif et propre (corrigé avec le code HEX pour crimson)
fviz_ca_biplot(res2.ca, 
               select.row = list(cos2 = 0.8),  # Filtre les lignes (individus/catégories)
               select.col = list(cos2 = 0.8),  # Filtre les colonnes (modes d'obtention)
               geom.row = c("point", "text"),  # FORCE l'affichage des points ET du texte pour les lignes
               geom.col = c("point", "text"),  # FORCE l'affichage des points ET du texte pour les colonnes
               repel = TRUE,                   # Empêche les textes de se chevaucher
               col.row = "royalblue", 
               col.col = "#DC143C",
               ggtheme = theme_minimal()) +
  labs(title = "Cartographie AFC — Profils filtrés (cos² > 0.8)",
       x = "Dim 1 (26.11%)", 
       y = "Dim 2 (22.35%)")

#Interpretation:
#Ce plan factoriel épuré ($\cos^2 > 0.8$) met en lumière le principal clivage économique du jeu sur la Dimension 1 (26,11 %). L'axe oppose horizontalement deux modèles d'acquisition : à l'extrême droite, un pôle d'exclusivité associant fortement les voitures populaires rétro (CULTCL) aux récompenses à durée limitée (Season Event) ; à l'inverse, la partie gauche regroupe le cœur commercial standard où les catégories comme HYPER ou RETRO s'alignent directement sur l'achat classique au salon de l'auto (Autoshow)

# EXPLORATION DES AXES SUPPLÉMENTAIRES (Axe 3) 

# On regarde si l'axe 3 apporte une information supplémentaire intéressante
round(sort(res2.ca$row$contrib[,3], decreasing=T),3) 
fviz_contrib(res2.ca, choice = "row", axe=3)
#Interpretation: 
#Ce graphique montre que la Dimension 3 est quasi exclusivement construite par la catégorie CLASC (Classic Sports Cars), qui écrase l'axe avec près de 40 % de contribution à elle seule. Les autres catégories historiques comme CULTCA (Cult Cars), VINTR (Vintage Rally) et CLASM (Classic Muscle) franchissent également le seuil (ligne rouge), ce qui montre que ce troisième axe isole spécifiquement le segment des voitures de collection et anciennes du jeu.

# Croisement des différents axes (1 vs 3, 2 vs 3, etc.)
plot(res2.ca, selectCol="cos2 0.7", selectRow="cos2 0.7", axe=c(1,2))
plot(res2.ca, selectCol="cos2 0.7", selectRow="cos2 0.7", axe=c(1,3))
plot(res2.ca, selectCol="cos2 0.7", selectRow="cos2 0.7", axe=c(2,3))
#Interpretation:

# ATYPIE RELATIVE

# L’atypie relative mesure le rapport entre l’inertie d’un point et son poids (marge).
# Plus cette valeur est élevée, plus la catégorie s’écarte du comportement moyen
# et contribue de manière spécifique à la structure du tableau.
sort(res2.ca$row$inertia / res2.ca$call$marge.row)
sort(res2.ca$col$inertia / res2.ca$call$marge.col)
#Interpretation:

# --- 5.6 Carte factorielle principale ---
p_afc_main <- fviz_ca_biplot(res2.ca,
                             repel     = TRUE,
                             col.row   = "#E53935",
                             col.col   = "#1E88E5",
                             ggtheme   = theme_minimal(),
                             title     = "Carte AFC — Model_type x car_source",
                             subtitle  = "Rouge = catégorie  |  Bleu = mode d'obtention",
                             labelsize = 3.5)
print(p_afc_main)
#Interpretation:
#Analyse du plan factoriel 1-2 (48,4 % de la variance totale) : La projection des données révèle que la distribution du catalogue s'articule autour d'un dualisme fort entre accessibilité et exclusivité, opposant horizontalement un pôle événementiel unique où les voitures rétro populaires (CULTCL) sont structurellement captives des récompenses à durée limitée (Season Event), à un axe vertical de progression où les engins lourds ou de niche (TRACT, RALLM) s'obtiennent au mérite via les distinctions du joueur (Accolade), tandis que l'essentiel des catégories standards reste concentré au centre de gravité sous la dépendance exclusive de l'achat direct au salon de l'auto (Autoshow).

# --- 5.7 AFC avec Drive_Type en éléments supplémentaires ---
# Drive_Type est projeté sans influencer les axes
tab_sup <- rbind(tab_contingence,
                 table(car_afc$Drive_Type, car_afc$car_source))

n_actif <- nrow(tab_contingence)
n_total  <- nrow(tab_sup)

res.ca_sup <- CA(tab_sup, row.sup = (n_actif + 1):n_total, graph = FALSE)

summary(res.ca_sup)
res.ca_sup$row.sup

p_afc_sup <- fviz_ca_biplot(res.ca_sup,
                            repel       = TRUE,
                            col.row     = "#E53935",
                            col.col     = "#1E88E5",
                            col.row.sup = "#FF8F00",
                            ggtheme     = theme_minimal(),
                            title       = "AFC + Drive_Type en éléments supplémentaires",
                            subtitle    = "Orange = Drive_Type projeté",
                            labelsize   = 3.5)
print(p_afc_sup)

#Interpretation:
#Analyse de l'AFC avec projection du type de transmission (48,4 % de la variance) : L'intégration de la transmission (Drive_Type) en variable illustrative montre que les propulsions (RWD, à gauche) s'alignent plutôt sur les méthodes d'obtention alternatives et les voitures anciennes, tandis que les quatre roues motrices (AWD, au centre-droit) gravitent près du salon de l'auto (Autoshow) et des défis saisonniers (Season Event), confirmant que le mode de transmission suit la même séparation logique que le catalogue général, sans pour autant perturber la forte opposition horizontale entre l'exclusivité des modèles CULTCL et la standardisation des catégories centrales.

# ==============================================================================
# FIN DU SCRIPT
# ==============================================================================
# Objets principaux :
#   car_clean_2     -> données nettoyées
#   data_quant      -> variables quantitatives imputées
#   res.pca         -> résultats ACP
#   tab_contingence -> tableau de contingence Model_type x car_source
#   chi2_test       -> test Chi-deux
#   res.ca          -> résultats AFC
#   res.ca_sup      -> AFC avec Drive_Type en supplémentaire
# ==============================================================================