#!/bin/bash

ORBIT=${1:=10000}
PS_PREFIX="current_orb${ORBIT}"

echo "Using prefix ${PS_PREFIX}..."

echo "pdfunite \\" > makepdfs.sh

for (( i = 0; i < 142; i++ )) 
do 
    ps2pdf ${PS_PREFIX}_${i}.ps 
    echo " ${PS_PREFIX}_${i}.pdf \\" >> makepdfs.sh 
    rm ${PS_PREFIX}_${i}.ps
done

echo " total_${PS_PREFIX}.pdf" >> makepdfs.sh

chmod 775 makepdfs.sh

echo "Made and chmodded makepdfs.sh"
