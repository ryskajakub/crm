#!/bin/bash

declare -a COMPANIES=("Continental" "České dráhy" "FOMA Bohemia" "Kand" "Metrostav" "Neumann" "PREX" "Stachema Kolín" "Valsabbia")

for COMPANY in "${COMPANIES[@]}" ; do
  STR="{\"phone\":\"721 650 194\",\"plant\":\"I\",\"contact\":\"p.Jelínek\",\"address\":\"Brandýs nad labem\",\"name\":\"$COMPANY\"}"
  curl -X POST -H "Content-Type: application/json" -d "${STR}" http://crm/api/v1.0.0/company
done
