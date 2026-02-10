# Clustering des employés – Analyse et segmentation

## 1️⃣ Objectif

Ce projet a pour objectif de **segmenter les employés d’un dataset fictif** en groupes distincts selon leurs caractéristiques socio-économiques et professionnelles (âge, revenu mensuel, ancienneté, dépense annuelle, type de contrat, secteur, région).  
Le but est de mettre en évidence des **profils types d’employés** et d’illustrer l’utilisation de **KMeans** avec un pipeline de prétraitement réutilisable sur d’autres jeux de données.

---

## 2️⃣ Préparation et exploration des données

1. **Chargement et nettoyage**
   - Suppression des doublons  
   - Imputation des valeurs manquantes : moyenne pour les variables numériques, mode pour les catégorielles

2. **Exploration**
   - Statistiques descriptives pour les variables numériques  
   - Visualisation des distributions avec des histogrammes  
   - Analyse des variables catégorielles avec des barplots  
   - Matrice de corrélation pour observer les relations entre variables numériques

---

## 3️⃣ Prétraitement pour le clustering

- **Standardisation** des variables numériques (pour que toutes aient le même poids)  
- **One-Hot Encoding** des variables catégorielles  
- Utilisation d’un **ColumnTransformer** pour appliquer ces transformations dans un pipeline cohérent

---

## 4️⃣ Détermination du nombre optimal de clusters

- La **méthode du coude** a été utilisée sur l’inertie de KMeans pour différents nombres de clusters (K=2 à 10)  
- Le graphique montre un **coude à K=3**, justifiant le choix de **3 clusters**  

---

## 5️⃣ Clustering avec KMeans

- Chaque employé a été assigné à un cluster (0, 1 ou 2)  
- Les clusters ont été ajoutés au dataset pour interprétation

---

## 6️⃣ Analyse des clusters

### Taille des clusters

| Cluster | Nombre d’employés |
|---------|-----------------|
| 0       | 118             |
| 1       | 107             |
| 2       | 75              |

### Profils moyens

| Cluster | Âge moyen | Revenu moyen | Ancienneté | Dépense annuelle moyenne |
|---------|-----------|--------------|------------|--------------------------|
| 0       | 42        | 178 000      | 6,85       | 1 041 527               |
| 1       | 38        | 271 775      | 14,55      | 1 748 434               |
| 2       | 42,65     | 330 896      | 4,40       | 1 927 705               |

### Interprétation des clusters

- **Cluster 0 – Jeunes profils moyens** : employés d’âge moyen avec faible revenu et ancienneté  
- **Cluster 1 – Seniors expérimentés** : employés plus expérimentés avec revenu et dépense intermédiaire à élevé  
- **Cluster 2 – Cadres hauts revenus récents** : employés avec revenu et dépense élevés mais ancienneté faible  

### Visualisation

- Un scatter plot montre la séparation des clusters selon **revenu mensuel vs dépense annuelle**, confirmant la cohérence des profils.

---

## 7️⃣ Conclusion

Le clustering a permis de **segmenter les employés en 3 groupes distincts**.  
Cette segmentation met en évidence des **profils types** qui peuvent être utilisés pour des analyses RH, économiques ou marketing interne.  
Le pipeline de prétraitement (standardisation + encodage) est **réutilisable pour d’autres datasets similaires**, garantissant des clusters fiables et comparables.
