#################################################################################################################################################################################################################################
title: "Projet gravity: Impact des tensions géopolitiques sur les flux commerciaux "
author: "Oumou Jasmine NGWAYA KANDE"
date: "2024-12-19"
#################################################################################################################################################################################################################################
  
#installer les packages
install.packages("readxl")
install.packages("tidyr")     # Pour le pivotement des données
install.packages("dplyr")
install.packages("tidyverse")
install.packages("sas7bdat")
install.packages("openxlsx")
install.packages("texreg")
install.packages("fixest")
install.packages("stargazer")
install.packages("plm")
install.packages("geosphere")
install.packages("haven")
install.packages("purrr")
install.packages("readr") # Installer readr
install.packages("lubridate")  # Pour gérer les dates


#appeler les packages
library(readxl)   # Pour lire les fichiers Excel
library(dplyr)    # Pour manipuler les données
library(tidyr)     # Pour le pivotement des données
library(tidyverse)
library(sas7bdat)
library(openxlsx)
library(texreg)
library(fixest)
library(stargazer)
library(plm)
library(geosphere)
library(purrr)    # Pour appliquer des fonctions aux listes
library(haven)
library(readr)
library(lubridate)  # Pour gérer les dates


####Importation des données####
setwd('C:/Users/OUMOU/Documents/doc/M2/fluxcommerciaux/Projet_gravity/Data_final') 

#####Premier traitement des données flux commerciaux#####
####données des USA

###ouvrir le fichier
chemin_dossier_USA<- "C:/Users/OUMOU/Documents/doc/M2/fluxcommerciaux/Projet_gravity/flux_commerciaux_2022_2012_USA/"# Chemin du dossier contenant les fichiers
fichiers_USA <- list.files(path = chemin_dossier_USA, pattern = "*.xlsx", full.names = TRUE)# Liste des fichiers Excel
print(fichiers_USA)# Afficher les fichiers trouvés

# Combiner tous les fichiers Excel
excel_sheets(fichiers_USA[1])
flux_commerciaux_2012_2022_USA <- fichiers_USA %>%
  map_dfr(~ read_excel(.x, sheet = "Partner"))# .x représente chaque fichier dans la liste

#Verification des données
head(flux_commerciaux_2012_2022_USA)# Vérifier l'aperçu des données
str(flux_commerciaux_2012_2022_USA)#Vérifier la structure complète des données
summary(flux_commerciaux_2012_2022_USA)#Résumé statistique des données
table(flux_commerciaux_2012_2022_USA$Year)#Vérification des données par année

# Filtrer les lignes pour les États-Unis comme "Reporter Name" et la Chine comme "Partner Name"
flux_commerciaux_2012_2022_USA <- flux_commerciaux_2012_2022_USA %>%
  select(
    `Reporter Name`,
    `Partner Name`,
    Year,
    `Export (US$ Thousand)`,
    `Import (US$ Thousand)`
  )

#ajouter la colonne country_code pour le code alphabétique du pays
flux_commerciaux_2012_2022_USA <- flux_commerciaux_2012_2022_USA %>%
  mutate(Contry_Code = "USA")  # Colonne Source = USA
head(flux_commerciaux_2012_2022_USA)


####données de la CHN

#ouvrir le fichier
chemin_dossier_CHN <- "C:/Users/OUMOU/Documents/doc/M2/fluxcommerciaux/Projet_gravity/flux_commerciaux_2022_2012_CHN/"# Chemin du dossier contenant les fichiers
fichiers_CHN <- list.files(path = chemin_dossier_CHN, pattern = "*.xlsx", full.names = TRUE)# Liste des fichiers Excel
print(fichiers_CHN)# Afficher les fichiers trouvés

# Combiner tous les fichiers Excel
excel_sheets(fichiers_CHN[1])
flux_commerciaux_2012_2022_CHN <- fichiers_CHN %>%
  map_dfr(~ read_excel(.x, sheet = "Partner"))# .x représente chaque fichier dans la liste

#Verification des données
head(flux_commerciaux_2012_2022_CHN)# Vérifier l'aperçu des données
str(flux_commerciaux_2012_2022_CHN)#Vérifier la structure complète des données
summary(flux_commerciaux_2012_2022_CHN)#Résumé statistique des données
table(flux_commerciaux_2012_2022_CHN$Year)#Vérification des données par année

# Filtrer les lignes pour les États-Unis comme "Reporter Name" et la Chine comme "Partner Name"
flux_commerciaux_2012_2022_CHN <- flux_commerciaux_2012_2022_CHN %>%
  select(
    `Reporter Name`,
    `Partner Name`,
    Year,
    `Export (US$ Thousand)`,
    `Import (US$ Thousand)`
  )

# Ajouter une colonne country_code
flux_commerciaux_2012_2022_CHN <- flux_commerciaux_2012_2022_CHN %>%
  mutate( Contry_Code= "CHN")  # Colonne Source = CHN

head(flux_commerciaux_2012_2022_CHN)


#### combiner les deux tables usa et chn

# Joindre les deux fichiers
flux_commerciaux_2012_2022 <- bind_rows(flux_commerciaux_2012_2022_CHN,
                                        flux_commerciaux_2012_2022_USA)
head(flux_commerciaux_2012_2022)# Vérifier les premières lignes

write.xlsx(flux_commerciaux_2012_2022, "flux_commerciaux.xlsx")












#####Premier traitement des données PIB en valeur courante #####

# Lire les données
fichier_Val_PIB <- "C:/Users/OUMOU/Documents/doc/M2/fluxcommerciaux/Projet_gravity/PIB_Valeur_courrente.xls"  # Remplacez par le chemin réel
donnees_Val_PIB <- read_xls(fichier_Val_PIB, skip = 3)    # Supposons que les 3 premières lignes ne sont pas des données

# Supprimer la colonne "Indicator Code" et les années 1960 à 2011, ainsi que 2023
donnees_Val_PIB <- donnees_Val_PIB %>%
  select(-`Indicator Code`,         # Supprimer la colonne "Indicator Code"
         -starts_with("196"),       # Supprimer les années 1960 à 1969
         -starts_with("197"),       # Supprimer les années 1970 à 1979
         -starts_with("198"),       # Supprimer les années 1980 à 1989
         -starts_with("199"),       # Supprimer les années 1990 à 1999
         -starts_with("200"),       # Supprimer les années 2000 à 2009
         -`2010`:-`2011`,           # Supprimer explicitement 2010 et 2011
         -`2023`)                   # Supprimer l'année 2023

head(donnees_Val_PIB)
str(donnees_Val_PIB)
summary(donnees_Val_PIB)#Résumé statistique des données

# Étape 1 : Transformer les années en lignes
donnees_pivot2 <- donnees_Val_PIB %>%
  pivot_longer(
    cols = `2012`:`2022`,       # Colonnes d'années à pivoter
    names_to = "Year",          # Nouvelle colonne pour les années
    values_to = "Value"         # Nouvelle colonne pour les valeurs associées
  )

# Étape 2 : Transformer les "Indicator Name" en colonnes distinctes
Val_PIB <- donnees_pivot2 %>%
  pivot_wider(
    names_from = `Indicator Name`,  # Les valeurs de "Indicator Name" deviennent des colonnes
    values_from = Value             # Les valeurs associées remplissent ces colonnes
  )


head(Val_PIB)# Afficher les premières lignes pour vérifier

write.xlsx(Val_PIB, "PIB_total.xlsx")















#####Premier traitement des données Tension #####

# Lire les données
fichier_tension <- "C:/Users/OUMOU/Documents/doc/M2/fluxcommerciaux/Projet_gravity/Tension_USA_CHN.csv"  # Remplacez par le chemin réel
donnees_tension <- read_csv(fichier_tension, skip = 1, col_names = c("date", "UTC"))  # Ignorer la 1ère ligne

#Nettoyer les colonnes
donnees_tension <- donnees_tension %>%
  mutate(
    Years = substr(date, 1, 4),  # Extraire les 4 premiers caractères pour l'année
    Tension_USA_CHN = UTC
  ) %>%
  select(Years, Tension_USA_CHN)    # Garder uniquement les colonnes nécessaires

# Filtrer les années de 2012 à 2022 uniquement
donnees_tension <- donnees_tension %>%
  filter(Years >= "2012" & Years <= "2022")

#Agréger les données pour calculer la moyenne des tensions par année
Tension <- donnees_tension %>%
  group_by(Years) %>%
  summarise(Tension_USA_CHN = mean(Tension_USA_CHN, na.rm = TRUE), .groups = "drop")

head(Tension)
str(Tension)

write.xlsx(Tension, "Tension.xlsx")











#####Premier traitement des données Sanctions#####

#ouvrir le fichier
chemin_Sanctions <- "C:/Users/OUMOU/Documents/doc/M2/fluxcommerciaux/Projet_gravity/Sanction_2012_2022/"# Chemin du dossier contenant les fichiers
fichiers_Sanctions <- list.files(path = chemin_Sanctions, pattern = "*.xlsx", full.names = TRUE)# Liste des fichiers Excel
print(fichiers_Sanctions)# Afficher les fichiers trouvés

# Combiner tous les fichiers Excel
excel_sheets(fichiers_Sanctions[1])

Sanctions_2012_2022 <- fichiers_Sanctions %>%
  map_dfr(~ read_excel(.x, sheet = "Data", col_types = "text"))# Lire les fichiers avec forçage des colonnes problématiques en texte

#Verification des données
head(Sanctions_2012_2022)# Vérifier l'aperçu des données
str(Sanctions_2012_2022)#Vérifier la structure complète des données
summary(Sanctions_2012_2022)#Résumé statistique des données

# Convertir les colonnes de date
Sanctions_2012_2022 <- Sanctions_2012_2022 %>%
  mutate(
    `Announcement Date` = as_date(as.numeric(`Announcement Date`), origin = "1899-12-30"),
    `Inception Date` = as_date(as.numeric(`Inception Date`), origin = "1899-12-30"),
    `Removal Date` = as_date(as.numeric(`Removal Date`), origin = "1899-12-30")
  )


# Filtrer les années de 2012 à 2022
Sanctions_2012_2022 <- Sanctions_2012_2022 %>%
  filter(year(`Announcement Date`) >= 2012 & year(`Announcement Date`) <= 2022)

# Créer une colonne Year
Sanctions_2012_2022 <- Sanctions_2012_2022 %>%
  mutate(Year = year(`Announcement Date`))

# Séparer la colonne "State Act Title" en deux colonnes
Sanctions_2012_2022 <- Sanctions_2012_2022 %>%
  separate(`State Act Title`, 
           into = c("State Concerned", "Intervention Description"),  # Noms des nouvelles colonnes
           sep = ":",                      # Séparateur à utiliser (ici ":")
           remove = TRUE,                  # Supprimer la colonne originale
           extra = "merge")                # Fusionner les valeurs restantes si le séparateur apparaît plusieurs fois

# Sélectionner les colonnes spécifiées
Sanctions_2012_2022 <- Sanctions_2012_2022 %>%
  filter(
    (`State Concerned` == "China" & `Affected Jurisdiction` == "United States of America") |
      (`State Concerned` == "United States of America" & `Affected Jurisdiction` == "China")
  ) %>%
  select(
    `State Concerned`,
    `Year`,
    `Intervention Description`,
    `Announcement Date`,
    `Inception Date`,
    `Currently in force`,
    `Intervention Type`,
    `MAST chapter`,
    `Affected Sectors`,
    `Affected Jurisdiction`
  )


# Regrouper par année ET par pays concerné/juridiquement affecté
Sanctions_2012_2022 <- Sanctions_2012_2022 %>%
  group_by(Year, `State Concerned`, `Affected Jurisdiction`) %>%
  summarise(
    Total_Sanctions = n(),
    Active_Sanctions = sum(`Currently in force` == "Yes", na.rm = TRUE),
    .groups = "drop"  # Supprimer les groupements pour éviter les bugs
  )

# Vérification des premières lignes
head(Sanctions_2012_2022)


write.xlsx(Sanctions_2012_2022, "Sanctions.xlsx")




####importation des données distances####
dist <- read_xls("total2.xls", sheet = "dist")    # Supposons que les 3 premières lignes ne sont pas des données
dist <- dist[c("isoi", "isoj", "dist", "contig", "colony")]# codes pays, distance, contiguïté (frontières partagées), relations coloniale.
head(dist)

####Importation des donnéés correspondance####
correspays <- read_xls("total2.xls", sheet = "correspays")    # Supposons que les 3 premières lignes ne sont pas des données
correspays <- correspays %>% select(-iso_old) # iso, L3, country  
correspays <- correspays %>% rename(i = iso, isoi = L3) #iso devient i. L3 devient isoi.
head(correspays)

#### Fusionner les bases de données distance et correspondance####
dist2 <- merge(dist, correspays, by = "isoi") # jointure entre dist et correspays sur la colonne isoi
dist3 <- dist %>% anti_join(dist2, by = "isoi")#  Identifie les lignes dans dist qui n'ont pas trouvé de correspondance dans correspays: CCK, CXR, PAL,PCN, TMP, YUG,ZAR

####nettoyage des données apres fusion qui empeche une fusion complete####
correspays <- correspays %>% 
  add_row(i = "166", isoi = "CCK", country = "Keeling") %>% 
  add_row(i = "162", isoi = "CXR", country = "Christmas Island") %>% 
  add_row(i = "612", isoi = "PCN", country = "Pitcairn Islands")

dist$isoi[dist$isoi == "PAL"] <- "PLW"
dist$isoj[dist$isoj == "PAL"] <- "PLW"

dist$isoi[dist$isoi == "TMP"] <- "TLS"
dist$isoj[dist$isoj == "TMP"] <- "TLS"

dist$isoi[dist$isoi == "YUG"] <- "SCG"
dist$isoj[dist$isoj == "YUG"] <- "SCG"

dist$isoi[dist$isoi == "ZAR"] <- "COD"
dist$isoj[dist$isoj == "ZAR"] <- "COD"

#### Fusionner les nouvelles bases de données dist et correspays####
dist <- merge(dist, correspays, by = "isoi") #Fusionne à nouveau dist avec la table corrigée de correspondance pour obtenir une table complète.
dist <- dist %>% rename(countryi = country)
corres <- correspays %>% rename(j = i, isoj = isoi, countryj = country) #Renomme les colonnes pour éviter des conflits lors des prochaines jointures (countryi pour le pays exportateur, countryj pour l'importateur).

dist_iso <- merge(corres, dist, by = "isoj")
dist_iso$i <- as.numeric(dist_iso$i) # Convertit les identifiants de pays en numériques pour harmoniser les types de données.
dist_iso$j <- as.numeric(dist_iso$j)
head(dist_iso)

####Second traitement des données flux commerciaux####

#importation du fichier
flux <- read_excel("flux_commerciaux.xlsx")

#modifications des valeurs des colonnes
flux <- flux %>% 
  mutate(
    `Reporter Name` = ifelse(`Reporter Name` == "United States", "United states of america", `Reporter Name`),
    `Partner Name` = ifelse(`Partner Name` == "United States", "United states of america", `Partner Name`)
  )  %>%
  rename(countryj = `Reporter Name`, country = `Partner Name`, t = Year, V_Export = `Export (US$ Thousand)`,V_Import = `Import (US$ Thousand)`, isoj = Contry_Code)
head(flux)

####harmoniser les données de flux et de correspays
harmoniser_noms <- function(flux, correspays, colonne_flux, colonne_correspays) {
  # Identifier les noms différents entre les deux tables
  noms_differents <- setdiff(unique(flux[[colonne_flux]]), unique(correspays[[colonne_correspays]]))
  
  # Créer une table de correspondance à compléter manuellement
  correspondances <- tibble::tibble(
    flux_name = noms_differents,  # Noms dans la table flux
    correspays_name = NA          # Noms correspondants dans correspays (à compléter)
  )
  
  # Retourner la liste des correspondances à compléter
  return(correspondances)
}

appliquer_harmonisation <- function(flux, correspondances, colonne_flux) {
  # Appliquer les correspondances sur la colonne de flux
  flux <- flux %>%
    left_join(correspondances, by = setNames("flux_name", colonne_flux)) %>%
    mutate(
      !!sym(colonne_flux) := ifelse(
        is.na(correspays_name), !!sym(colonne_flux), correspays_name
      )
    ) %>%
    select(-correspays_name) # Supprimer la colonne temporaire
  
  return(flux)
}

correspondances_a_completer <- harmoniser_noms(
  flux = flux,
  correspays = correspays,
  colonne_flux = "country",
  colonne_correspays = "country"
)
print(correspondances_a_completer)

correspondances_completes <- correspondances_a_completer %>%
  mutate(
    correspays_name = case_when(
      flux_name == "United Arab Emirates" ~ "United arab emirates",
      flux_name == "Bahamas, The" ~ "Bahamas",
      flux_name == "Cote d'Ivoire" ~ "Côte d'ivoire",
      flux_name == "Congo, Rep." ~ "Congo",
      flux_name == "Antigua and Barbuda" ~ "Antigua and barbuda",
      flux_name == "Bosnia and Herzegovina" ~ "Bosnia and herzegovina",
      flux_name == "Brunei" ~ "Brunei darussalam",
      flux_name == "Central African Republic" ~ "Central african republic",
      flux_name == "Cape Verde" ~ "Cape verde",
      flux_name == "Costa Rica" ~ "Costa rica",
      flux_name == "Cayman Islands" ~ "Cayman islands",
      flux_name == "Czech Republic" ~ "Czech republic",
      flux_name == "Dominican Republic" ~ "Dominican republic",
      flux_name == "Egypt, Arab Rep." ~ "Egypt",
      flux_name == "Ethiopia(excludes Eritrea)" ~ "Ethiopia",
      flux_name == "Faeroe Islands" ~ "Faroe islands",
      flux_name == "Micronesia, Fed. Sts." ~ "Micronesia, fed. st.",
      flux_name == "United Kingdom" ~ "United kingdom",
      flux_name == "Gambia, The" ~ "Gambia",
      flux_name == "Guinea-Bissau" ~ "Guinea-bissau",
      flux_name == "Equatorial Guinea" ~ "Equatorial guinea",
      flux_name == "Hong Kong, China" ~ "Hong kong",
      flux_name == "Iran, Islamic Rep." ~ "Iran",
      flux_name == "Kazakhstan" ~ "Kazakstan",
      flux_name == "Kyrgyz Republic" ~ "Kyrgyzstan",
      flux_name == "St. Kitts and Nevis" ~ "Saint kitts and nevis",
      flux_name == "Korea, Rep." ~ "Korea",
      flux_name == "Lao PDR" ~ "Lao people's democratic",
      flux_name == "St. Lucia" ~ "Saint lucia",
      flux_name == "Sri Lanka" ~ "Sri lanka",
      flux_name == "Marshall Islands" ~ "Marshall islands",
      flux_name == "North Macedonia" ~ "Macedonia",
      flux_name == "Papua New Guinea" ~ "Papua new guinea",
      flux_name == "Russian Federation" ~ "Russian federation",
      flux_name == "San Marino" ~ "San marino",
      flux_name == "Sao Tome and Principe" ~ "Sao tome and principe",
      flux_name == "Slovak Republic" ~ "Slovakia",
      flux_name == "Eswatini" ~ "Swaziland",
      flux_name == "Syrian Arab Republic" ~ "Syrian arab republic",
      flux_name == "Trinidad and Tobago" ~ "Trinidad and tobago",
      flux_name == "St. Vincent and the Grenadines" ~ "Saint vincent and the gr",
      flux_name == "Vietnam" ~ "Viet nam",
      flux_name == "Congo, Dem. Rep." ~ "Congo (democratic rep.)",
      flux_name == "Cocos (Keeling) Islands" ~ "Cocos (keeling) islands",
      flux_name == "Falkland Island" ~ "Falkland islands",
      flux_name == "British Indian Ocean Ter." ~ "British indian ocean ter.",
      TRUE ~ correspays_name
    )
  )

flux_harmonise <- appliquer_harmonisation(
  flux = flux,
  correspondances = correspondances_completes,
  colonne_flux = "country"
)

print(setdiff(unique(flux_harmonise$country), unique(correspays$country)))

correspondances_completes <- correspondances_completes %>%
  mutate(
    correspays_name = case_when(
      flux_name == "Bonaire" ~ "Netherlands",
      flux_name == "Burkina Faso" ~ "Burkina faso",
      flux_name == "Cook Islands" ~ "Cook islands",
      flux_name == "Curaçao" ~ "Netherlands antilles",
      flux_name == "East Asia & Pacific" ~ NA,  # Non applicable
      flux_name == "Europe & Central Asia" ~ NA,  # Non applicable
      flux_name == "Western Sahara" ~ "Western sahara",
      flux_name == "Libya" ~ "Libyan arab jamahiriya",
      flux_name == "Latin America & Caribbean" ~ NA,  # Non applicable
      flux_name == "Macao" ~ "Macau",
      flux_name == "Moldova" ~ "Moldova, rep.of",
      flux_name == "Middle East & North Africa" ~ NA,  # Non applicable
      flux_name == "Montenegro" ~ "Serbia and Montenegro",
      flux_name == "North America" ~ NA,  # Non applicable
      flux_name == "New Caledonia" ~ "New caledonia",
      flux_name == "New Zealand" ~ "New zealand",
      flux_name == "Other Asia, nes" ~ NA,  # Non applicable
      flux_name == "Korea, Dem. Rep." ~ "Korea, dem. people's rep",
      flux_name == "Occ.Pal.Terr" ~ "Palestinian territory",
      flux_name == "French Polynesia" ~ "French polynesia",
      flux_name == "South Asia" ~ NA,  # Non applicable
      flux_name == "Saudi Arabia" ~ "Saudi arabia",
      flux_name == "Serbia, FR(Serbia/Montenegro)" ~ "Serbia and Montenegro",
      flux_name == "Solomon Islands" ~ "Solomon islands",
      flux_name == "Sierra Leone" ~ "Sierra leone",
      flux_name == "El Salvador" ~ "El salvador",
      flux_name == "Saint Pierre and Miquelon" ~ "St. pierre and miquelon",
      flux_name == "South Sudan" ~ "South Sudan",  # Correspondance directe
      flux_name == "Sub-Saharan Africa" ~ NA,  # Non applicable
      flux_name == "Turks and Caicos Isl." ~ "Turks and caicos islands",
      flux_name == "East Timor" ~ "East timor",
      flux_name == "Unspecified" ~ NA,  # Non applicable
      flux_name == "Holy See" ~ "Holy See",  # Correspondance directe
      flux_name == "British Virgin Islands" ~ "Virgin islands (british)",
      flux_name == "World" ~ NA,  # Non applicable
      flux_name == "South Africa" ~ "South africa",
      flux_name == "Wallis and Futura Isl." ~ "Wallis and futuna island",
      flux_name == "Anguila" ~ "Anguilla",
      flux_name == "Fr. So. Ant. Tr" ~ "French southern territories",
      flux_name == "Cocos (keeling) islands" ~ "Cocos (keeling) islands",
      flux_name == "Heard Island and McDonald Isla" ~ "Heard and McDonald islands",
      flux_name == "British indian ocean ter." ~ "British indian ocean territory",
      flux_name == "Pitcairn" ~ "Pitcairn",
      flux_name == "Saint Maarten (Dutch part)" ~ "Netherlands antilles",
      TRUE ~ correspays_name
    )
  )

flux_harmonise <- appliquer_harmonisation(
  flux = flux,
  correspondances = correspondances_completes,
  colonne_flux = "country"
)

flux_harmonise <- flux_harmonise %>%
  filter(!country %in% c("Netherlands antilles","East Asia & Pacific","Europe & Central Asia","Latin America & Caribbean","Middle East & North Africa","North America","Other Asia, nes","South Asia","South sudan","Sub-Saharan Africa","Unspecified","Holy See","World","French southern territories","Cocos (keeling) islands","Heard and McDonald islands","British indian ocean ter.","Pitcairn"   ))

#### joindre correspays et flux_harmonise

flux_iso <- merge(flux_harmonise, correspays, by = "country") #Fusionne les flux commerciaux (flux) avec les distances bilatérales (dist_iso) par les pays
flux_iso_verif <- flux_harmonise %>% anti_join(flux_iso, by = "country")
head(flux_iso_verif)

flux_iso <- flux_iso %>% rename(countryi = country)

#### joindre flux_iso et dist_iso####

flux_dist <- merge(flux_iso, dist_iso, by = c("isoi","isoj"))
flux_dist_verif <- flux_iso %>% anti_join(flux_dist, by = c("isoi","isoj"))
head(flux_dist_verif)

unique_problems <- flux_dist_verif %>%
  select(countryi, countryj, isoi, isoj) %>%
  distinct()

print(unique_problems)

missing_isoi <- setdiff(unique(flux_iso$isoi), unique(dist_iso$isoi))
missing_isoj <- setdiff(unique(flux_iso$isoj), unique(dist_iso$isoj))

print(missing_isoi)  # Codes `isoi` manquants
print(missing_isoj)  # Codes `isoj` manquants

####Calculer la distance géographique

# Coordonnées approximatives
coords <- data.frame(
  isoi = c("MYT", "PSE", "CHN", "USA"),
  country = c("Mayotte", "Palestinian territory", "China", "United states of america"),
  lat = c(-12.833, 31.952, 39.916, 38.907),  # Latitude
  lon = c(45.166, 35.233, 116.383, -77.037)  # Longitude
)

# Extraire les coordonnées pour chaque combinaison
get_distance <- function(from, to, data) {
  coords_from <- data[data$isoi == from, c("lon", "lat")]
  coords_to <- data[data$isoi == to, c("lon", "lat")]
  dist <- distHaversine(as.matrix(coords_from), as.matrix(coords_to)) / 1000  # Distance en km
  return(dist)
}

# Ajouter des lignes avec les distances calculées
dist_iso <- dist_iso %>%
  add_row(isoi = "MYT", isoj = "CHN", dist = get_distance("MYT", "CHN", coords), contig = 0, colony = 0, countryi = "Mayotte", countryj = "China") %>%
  add_row(isoi = "MYT", isoj = "USA", dist = get_distance("MYT", "USA", coords), contig = 0, colony = 0, countryi = "Mayotte", countryj = "United states of america") %>%
  add_row(isoi = "PSE", isoj = "USA", dist = get_distance("PSE", "USA", coords), contig = 0, colony = 0, countryi = "Palestinian territory", countryj = "United states of america") %>%
  add_row(isoi = "PSE", isoj = "CHN", dist = get_distance("PSE", "CHN", coords), contig = 0, colony = 0, countryi = "Palestinian territory", countryj = "China")
## ces distance seront à cerifier ulterieurement

#### joindre à nouveau flux_iso et dist_iso

flux_dist <- merge(flux_iso, dist_iso, by = c("isoi","isoj"))
flux_dist_verif <- flux_iso %>% anti_join(flux_dist, by = c("isoi","isoj"))
head(flux_dist_verif)

#supprimer les variables redondantes
flux_dist <- flux_dist %>% 
  rename(i = i.x, countryj = countryj.x, countryi = countryi.x)%>% 
  select(-i.y,-countryi.y,-countryj.y)
head(flux_dist)

####second traitement des données PIB en valeur courante####

#Importation des données
pib <- read_excel("PIB_total.xlsx")
pib <- pib %>% rename(yi = `GDP (current US$)`, country = `Country Name`, isoi = `Country Code`,t=Year) # Crée une table des PIB pour les pays exportateurs (yi)

#### joindre PIB et corres  

pib_iso <- merge(pib, correspays, by = "isoi")
pib_iso_verif <- pib %>% anti_join(pib_iso, by = "isoi")
head(pib_iso_verif)

unique_problems <- pib_iso_verif %>%
  select(country) %>%
  distinct()

print(unique_problems)

## supprimer les lignes unitiles pour mon projet
pib <- pib %>% filter(!country %in% c("Africa Eastern and Southern",
                                      "Africa Western and Central",
                                      "Arab World",
                                      "Central Europe and the Baltics",
                                      "Channel Islands",
                                      "Caribbean small states",
                                      "Curacao",
                                      "East Asia & Pacific (excluding high income)",
                                      "Early-demographic dividend","East Asia & Pacific",
                                      "Europe & Central Asia (excluding high income)",
                                      "Europe & Central Asia",
                                      "Euro area",
  "European Union",
  "Fragile and conflict affected situations",
  "High income",
  "Heavily indebted poor countries (HIPC)",
  "IBRD only",
  "IDA & IBRD total",
  "IDA total",
  "IDA blend",
  "IDA only",
  "Isle of Man",
  "Not classified",
  "Latin America & Caribbean (excluding high income)",
  "Latin America & Caribbean",
  "Least developed countries: UN classification",
  "Low income",
  "Lower middle income",
  "Low & middle income",
  "Late-demographic dividend",
  "St. Martin (French part)",
  "Middle East & North Africa",
  "Middle income",
  "Middle East & North Africa (excluding high income)",
  "Montenegro",
  "North America",
  "OECD members",
  "Other small states",
  "Pre-demographic dividend",
  "Pacific island small states",
  "Post-demographic dividend",
  "Romania",
  "South Asia",
  "Serbia",
  "Sub-Saharan Africa (excluding high income)",
  "South Sudan",
  "Sub-Saharan Africa",
  "Small states",
  "Sint Maarten (Dutch part)",
  "East Asia & Pacific (IDA & IBRD countries)",
  "Europe & Central Asia (IDA & IBRD countries)",
  "Latin America & the Caribbean (IDA & IBRD countries)",
  "Middle East & North Africa (IDA & IBRD countries)",
  "South Asia (IDA & IBRD)",
  "Sub-Saharan Africa (IDA & IBRD countries)",
  "Upper middle income",
  "World",
  "Kosovo"
)
)

#### joindre à nouveau PIB et corres pour etre sur que les variables sont les memes

pib_iso <- merge(pib, correspays, by = "isoi")
pib_iso_verif <- pib %>% anti_join(pib_iso, by = "isoi")
head(pib_iso_verif)

pibi <- pib_iso %>% 
  rename(countryi = country.x)%>% 
  select(-country.y)
head(pibi)

pibj <- pib_iso %>% 
  rename(countryj = country.x,isoj=isoi,yj=yi,j=i)%>% 
  select(-country.y)
head(pibj)

####Fusionne les données de PIB des exportateurs et des importateurs avec la base flux_dist.
merge2 <- merge(flux_dist, pibi, by = c("countryi", "t"))
merge3 <- merge(merge2, pibj, by = c("countryj", "t"))
pib_flux_dist <- merge3
head(pib_flux_dist)

## supprimer et renommer les colonnes

pib_flux_dist <- pib_flux_dist %>% 
  rename(isoi=isoi.x,isoj=isoj.x,j=j.x,i=i.x)%>% 
  select(-isoi.y,-isoj.y,-i.y,-j.y)
head(pib_flux_dist)

####Second traitement des données tension ####

#importer le fichier
tension <- read_excel("Tension.xlsx")

tension <- tension %>% 
  rename(t=Years) 
head(tension)

#### joindre tension et pib_flux_dist####
tension_pib_flux_dist <- merge(tension, pib_flux_dist, by = "t")
tension_pib_flux_dist_verif <- tension %>% anti_join(tension_pib_flux_dist, by = "t")
head(tension_pib_flux_dist_verif)

#### Second traitement des données Sanction ####

sanctions <- read_excel("Sanctions.xlsx")
sanctions <- sanctions %>% 
  rename(t=Year,countryj=`Affected Jurisdiction`) %>%
  select(-`State Concerned` ,)#ne selectionner que `Affected Jurisdiction` car si un pays recoit une sanction c'est car l'autre dans `State Concerned` sera l'auteur

head(sanctions)

#### fusionner les données de sanctions et de tension_pib_flux_dist####
model <- merge(sanctions, tension_pib_flux_dist, by = c("t","countryj"))
head(model)

write.xlsx(model, "Donnees_complete_Tension_USA_CHN.xlsx")


####Estimation du modèle gravitaire####

# Filtrer pour garder uniquement les exportations vers USA ou CHN
# Transformation log des exportations et des importations
model <- read_excel("Donnees_complete_Tension_USA_CHN.xlsx") 

bilateral_USA_CHN <- model %>%
  filter(countryj %in% c("United states of america", "China"))

# Ajouter une variable d'indicateur pour USA et CHN
bilateral_USA_CHN <- bilateral_USA_CHN %>%
  mutate(bilateral = ifelse(countryi == "United states of america" & countryj == "China" |
                              countryi == "China" & countryj == "United states of america", 1, 0))

bilateral_USA_CHN <- bilateral_USA_CHN %>%
  mutate(
    lV_Export = log(V_Export + 1), # Transformation log pour éviter log(0)
    lV_Import = log(V_Import + 1),
    ldist = log(dist + 1),
    lyi = log(yi + 1),
    lyj = log(yj + 1)
  )

# Vérifier les données filtrées
head(bilateral_USA_CHN)


library(ggplot2)

#Analyse descriptive
# Visualiser l'évolution des exportations en fonction des tensions
ggplot(bilateral_USA_CHN, aes(x = t, y = V_Export, color = Tension_USA_CHN)) +
  geom_point() +
  geom_line() +
  labs(title = "Évolution des flux commerciaux en fonction des tensions géopolitiques",
       x = "Année",
       y = "Exportations (en milliers USD)",
       color = "Tension USA-CHN") +
  theme_minimal()


# Régression OLS
lm_ols <- lm(lV_Export ~ bilateral + Total_Sanctions + ldist + lyi + lyj, data = bilateral_USA_CHN)
summary(lm_ols)

library(plm)

# Modèle à effets fixes
# Modèle à effets fixes
lm_fixed <- plm(lV_Export ~ bilateral + Total_Sanctions + ldist + lyi + lyj,
                data = bilateral_USA_CHN,
                index = c("isoi", "isoj"),
                model = "within")

# Résumé du modèle
summary(lm_fixed)


# Résumé du modèle
summary(lm_fixed)

library(fixest)

# Modèle PPML
ppml <- feglm(V_Export ~ Tension_USA_CHN + Total_Sanctions + dist | isoi + isoj,
              data = bilateral_USA_CHN,
              family = poisson(link = "log"))

# Résumé du modèle
summary(ppml)

stargazer(lm_ols, lm_fixed, type = "text",
          title = "Impact des tensions géopolitiques sur les flux commerciaux",
          omit = c("isoi", "isoj"),
          add.lines = list(c("Effets fixes", "Non", "Oui")))

install.packages("modelsummary")
library(modelsummary)

# Créer une liste de modèles
model_list <- list(
  "OLS" = lm_ols,
  "Fixed Effects" = lm_fixed,
  "PPML" = ppml
)

# Afficher les résultats dans la console
modelsummary(model_list, title = "Impact des tensions géopolitiques sur les flux commerciaux")

modelsummary(model_list, title = "Impact des tensions géopolitiques sur les flux commerciaux", 
             output = "table.html")  # Changez "table.html" en "table.tex" ou "table.docx"

