# -*- coding: utf-8 -*-
"""
Éditeur de Spyder

Ceci est un script temporaire.
"""

# Importation des packages avec lesquelles je vais travailler
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import numpy as np




# Importer les fichiers 4G et population
Internet = pd.read_csv("C:/Users/houmo/Documents/universite/UPPA_2024-2025/économie géo/projet_graphe/2024_T3_4GF_Metropole_sites.csv", sep=";", encoding="latin1")
Population = pd.read_csv("C:/Users/houmo/Documents/universite/UPPA_2024-2025/économie géo/projet_graphe/DS_ESTIMATION_POPULATION_data.csv", sep=";", encoding="utf-8")

# afficher les détailles 
print(Internet.head())          # aperçu des données T3
print(Population.head())  # aperçu des données de population

print(Internet.columns.tolist())
print(Population.columns.tolist())

# Liste des départements uniques
departements_Internet = Internet["insee_dep"].unique()
print(departements_Internet)
print("Nombre de départements :", len(departements_Internet))

departements_pop = Population["GEO"].unique()
print(departements_pop)
print("Nombre de départements :", len(departements_pop))

# Convertir en ensembles pour comparaison
set_internet = set(departements_Internet)
set_pop = set(departements_pop)

# Départements dans Internet mais pas dans Population
print("Présents dans Internet mais pas dans Population :", set_internet - set_pop)

# Départements dans Population mais pas dans Internet
print("Présents dans Population mais pas dans Internet :", set_pop - set_internet)

#Internet
# Regroupement des sites 4G par département
internet_dep = Internet.groupby("insee_dep").agg({
    "sites_demandes": "sum",
    "sites_mes": "sum",
    "sites_6_mois": "sum",
    "sites_6_24_mois": "sum",
    "sites_attente_deploiement": "sum"
}).reset_index()

# Calcul du taux de déploiement (%)
internet_dep["taux_deploiement"] = internet_dep["sites_mes"] / internet_dep["sites_demandes"] * 100

# Vérification
print(internet_dep.head())

#Population
# Définir les tranches d'âge des 15-29 ans
jeunes = ["Y15T19", "Y20T24", "Y25T29"]

# Filtrer la base population pour ne garder que les jeunes
pop_jeunes = Population[
    (Population["AGE"].isin(jeunes)) &
    (Population["SEX"] == "_T") &
    (Population["EP_MEASURE"] == "POP") &
    (Population["TIME_PERIOD"] == 2023)
]

# Regrouper la population jeune par code département
pop_jeunes_dep = pop_jeunes.groupby("GEO")["OBS_VALUE"].sum().reset_index()
pop_jeunes_dep.columns = ["insee_dep", "pop_15_29"]

# Filtrer pour garder la population totale 
pop_totale = Population[
    (Population["AGE"] == "_T") &
    (Population["SEX"] == "_T") &
    (Population["EP_MEASURE"] == "POP") &
    (Population["TIME_PERIOD"] == 2023)
]

# Regrouper par département
pop_totale_dep = pop_totale.groupby("GEO")["OBS_VALUE"].sum().reset_index()
pop_totale_dep.columns = ["insee_dep", "pop_totale"]

# Fusion pop_15_29 et pop_totale
pop_merge = pd.merge(pop_jeunes_dep, pop_totale_dep, on="insee_dep", how="inner")

# Calcul de la part des jeunes en %
pop_merge["part_jeunes"] = (pop_merge["pop_15_29"] / pop_merge["pop_totale"]) * 100

# Vérification
print(pop_merge.head())


# Fusion des deux bases sur le code INSEE
DF = pd.merge(internet_dep, pop_merge, on="insee_dep", how="inner")

# Affichage d'un aperçu
print(DF.head())

# Nombre total de départements dans le tableau fusionné
print("Nombre de départements communs :", len(DF))

# Graphe
plt.figure(figsize=(20, 8))

# Scatter plot + courbe de tendance
sns.regplot(
    x=DF["part_jeunes"],
    y=DF["taux_deploiement"],
    scatter_kws={'alpha': 0.6, 's': 50},
    lowess=True,
    line_kws={'color': 'blue', 'linewidth': 2}
)

# Ajouter les étiquettes de chaque point
for i in range(len(DF)):
    x = DF["part_jeunes"].iloc[i]
    y = DF["taux_deploiement"].iloc[i]
    label = str(DF["insee_dep"].iloc[i])
    plt.text(x, y + 1, label, fontsize=8, ha='center', va='bottom', color='black')

# Personnalisation
plt.xlabel("Part des 15–29 ans (par département)")
plt.ylabel("Taux de déploiement 4G (%)")
plt.title("Lien entre population jeune et taux de déploiement 4G fixe (codes départements affichés)")
plt.grid(True)
plt.tight_layout()
plt.show()


# Calcul des positions des lignes de séparation pour les commentaires
x_median = np.median(DF["part_jeunes"])
y_median = np.median(DF["taux_deploiement"])

plt.figure(figsize=(20, 8))

# Nuage de points + tendance
sns.regplot(
    x=DF["part_jeunes"],
    y=DF["taux_deploiement"],
    scatter_kws={'alpha': 0.6, 's': 50},
    lowess=True,
    line_kws={'color': 'blue', 'linewidth': 2}
)

# Affichage des étiquettes (codes départements)
for i in range(len(DF)):
    x = DF["part_jeunes"].iloc[i]
    y = DF["taux_deploiement"].iloc[i]
    label = str(DF["insee_dep"].iloc[i])
    plt.text(x, y + 1, label, fontsize=8, ha='center', va='bottom', color='black')

# Ligne verticale (population médiane)
plt.axvline(x=x_median, color='red', linestyle='--', label='Médiane part des jeunes')

# Ligne horizontale (taux de déploiement médian)
plt.axhline(y=y_median, color='green', linestyle='--', label='Médiane taux de déploiement')

# Légendes et titres
plt.xlabel("Part des jeunes de 15–29 ans (par département)")
plt.ylabel("Taux de déploiement 4G (%)")
plt.title("Quadrillage : population jeune vs taux de déploiement 4G fixe")
plt.grid(True)
plt.legend()
plt.tight_layout()
plt.show()

DF.to_csv("C:/Users/houmo/Documents/universite/UPPA_2024-2025/économie géo/projet_graphe/df_final.csv", index=False, encoding="utf-8")


