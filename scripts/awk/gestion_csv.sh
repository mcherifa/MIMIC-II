#!/bin/sh

# Supprimer la ligne 2 de chaque csv
# qui contient les unités de mesure
# des variables enregistrées

for csv in *.csv;
do
sed -i '/mmHg/d' $csv; 
done

# Ajout colonne : ID avec en contenu
# le nom du fichier 

for csv in *.csv
do
awk 'NR==1{print $0 ",id"}NR>1{print $0 ","FILENAME}' "$csv" > tempfile && mv tempfile "$csv"
done 

# Enlever les espaces dans les colonnes
# et les remplacer par '_'

for csv in *.csv
do
awk 'NR==1{gsub(/ /, "_", $0); print} NR>1{print $0}' "$csv" > tempfile && mv tempfile "$csv"
done 

