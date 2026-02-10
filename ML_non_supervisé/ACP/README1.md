# Analyse en Composantes Principales (ACP) – Projet Data Science

## 🎯 Objectif du projet

Ce projet a pour objectif de réaliser une **Analyse en Composantes Principales (ACP)** sur un jeu de données d’individus afin de :  

- **Réduire la dimension** du jeu de données tout en conservant l’essentiel de l’information.  
- **Identifier les axes principaux** qui expliquent les variations entre les individus.  
- **Découvrir des groupes d’individus** partageant des caractéristiques similaires.  

---

## 📊 Résumé de l’ACP

L’ACP a été réalisée sur les variables numériques du dataset (`age`, `revenu_mensuel`, `anciennete_annees`, `depense_annuelle`).  

### 1️⃣ Axes factoriels retenus

Deux axes principaux expliquent une grande partie de la variation entre les individus :  

1. **PC1 – Niveau économique**  
   - Fortement lié au revenu mensuel et à la dépense annuelle.  
   - Permet de distinguer les individus avec un **niveau économique élevé ou faible**.  

2. **PC2 – Génération / expérience**  
   - Oppose l’âge (corrélation négative) à l’ancienneté (corrélation positive).  
   - Permet de distinguer les individus **jeunes ou âgés, peu ou très expérimentés**.  

### 2️⃣ Groupes d’individus identifiés

La projection des individus sur le plan formé par PC1 et PC2 permet de distinguer **quatre groupes principaux** :  

1. Individus âgés et expérimentés avec un **niveau économique élevé**  
2. Individus jeunes et peu expérimentés avec un **niveau économique élevé**  
3. Individus âgés et expérimentés avec un **faible niveau économique**  
4. Individus jeunes et peu expérimentés avec un **faible niveau économique**  

> Cette segmentation aide à visualiser rapidement les différences entre les individus et à mieux comprendre la structure globale du jeu de données.

---

## 📌 Utilisation du projet

- Le notebook contient toutes les étapes de l’ACP : **nettoyage des données, standardisation, calcul des axes, visualisations**.  
- Il peut servir de **modèle pour appliquer la même analyse à d’autres jeux de données** en adaptant simplement les variables et le fichier source.
