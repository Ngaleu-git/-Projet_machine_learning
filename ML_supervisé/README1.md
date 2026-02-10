# Prédiction de la dépense annuelle des clients (Machine Learning)

## 🎯 Objectif du projet
L’objectif de ce projet est de construire un **modèle de Machine Learning** capable de prédire la **dépense annuelle d’un client** à partir d'un jeu de données simulées.

Ce projet met en pratique toutes les étapes clés d’un projet Data Science :  
nettoyage des données, analyse exploratoire, préprocessing, modélisation et évaluation.

---

## 📊 Données utilisées
Le dataset contient des informations clients telles que :
- Âge
- Revenu mensuel
- Ancienneté (années)
- Type de contrat
- Secteur d’activité
- Région
- Dépense annuelle (variable cible)

---

## 🧹 Nettoyage et préparation des données
- Suppression des doublons  
- Traitement des valeurs manquantes  
- Gestion des valeurs aberrantes (revenus négatifs corrigés)  
- Séparation des variables numériques et catégorielles  

---

## 📈 Analyse exploratoire (EDA)
- Histogrammes des variables numériques  
- Barplots des variables catégorielles  
- Matrice de corrélation (heatmap)  
- Scatter plots entre les variables explicatives et la cible  
- Visualisation de la distribution de la dépense annuelle  

---

## ⚙️ Préprocessing
- Standardisation de certaines variables numériques  
- Normalisation Min-Max  
- Encodage One-Hot des variables catégorielles  
- Utilisation de `ColumnTransformer` et `Pipeline` pour éviter toute fuite de données  

---

## 🤖 Modélisation
- Modèle utilisé : **RandomForestRegressor**
- Entraînement via un pipeline complet
- Séparation Train / Test (80% / 20%)

---

## 📉 Évaluation du modèle
Les métriques utilisées :
- MAE (Mean Absolute Error)
- RMSE (Root Mean Squared Error)
- R² (coefficient de détermination)

**Résultat principal :**
- R² ≈ **0.57**, ce qui montre que le modèle explique une part significative de la variance de la dépense annuelle.

---

## 🔮 Prédiction sur de nouveaux clients
Le modèle permet de prédire la dépense annuelle de nouveaux clients à partir de leurs caractéristiques, ce qui peut aider à la **prise de décision** (marketing, segmentation, finance).

---

## 🛠️ Technologies utilisées
- Python  
- Pandas, NumPy  
- Matplotlib, Seaborn  
- Scikit-learn  

---

## 👤 Auteur
**Souleymane Daouda**  
Data Scientist Junior  

---

## 🚀 Améliorations possibles
- Optimisation des hyperparamètres  
- Gestion avancée des outliers  
- Comparaison avec d’autres modèles de régression  
- Déploiement du modèle (API, application web)

