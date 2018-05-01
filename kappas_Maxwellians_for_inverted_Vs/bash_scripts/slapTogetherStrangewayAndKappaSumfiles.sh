#!/bin/bash

# 2018/04/27
# NOTE: THE ORDER OF ORBIT PLOTS CATTED IS NOT NUMERICAL; IT IS BASED ON THE TIME OF CREATION (hence ls -lahtr)


dir=/SPENCEdata/software/sdt/batch_jobs/plots/20180425/kappa_fits/
PDFSUFF="_to300"
NPLOTSTOCAT=100
startOrb=5683

if [ ! -d "${dir}" ]; then
    echo "Can't! N'existe pas: ${dir}";
    exit
fi

cd ${dir}
CWD=$(pwd);

mapfile -t myArray < <(ls -lahtr | awk '{print $9}' | awk -v startOrb=${startOrb} -v nPlotsToCat=${NPLOTSTOCAT} 'BEGIN { orbDir="Orbit_" startOrb ; print orbDir; }; $0 ~ orbDir {for(i=1; i<=100; i++) {getline; print}}')

kappArr=();
swayArr=();

for bro in ${myArray[@]}; do
    orb=${bro##Orbit_}; echo ${orb};
    if [ -n "$(ls ${bro}/Kappa_summary*)" ]; then
	echo "${orb}: K!"; kappArr=(${kappArr[@]} $(ls ${bro}/Kappa_summary*) )
    fi
    if [ -n "$(ls ${bro}/Strangeway_summary*)" ]; then
	echo "${orb}: S!"; swayArr=(${swayArr[@]} $(ls ${bro}/Strangeway_summary*) )
    fi
    cd ${CWD}
done

# gs -sDEVICE=pdfwrite -dEPSFitPage -dNOPAUSE -dBATCH -dSAFER -sOutputFile=sWayOrbits${PDFSUFF}.pdf ${swayArr[@]}
# gs -sDEVICE=pdfwrite -dEPSFitPage -dNOPAUSE -dBATCH -dSAFER -sOutputFile=kappaOrbits${PDFSUFF}.pdf ${kappArr[@]}
