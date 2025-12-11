# -*- coding: utf-8 -*-
"""
Created on Mon Apr 29 15:44:31 2024

@author: Oumou Jasmine NGWAYA KANDE
"""

import pandas as pd

# Charger le fichier Excel pour analyser les données
file_path = '/mnt/data/Export_GTF_IEA_202402.xlsx'
data = pd.read_excel(file_path)

# Afficher les premières lignes du fichier pour comprendre sa structure
data.head()
