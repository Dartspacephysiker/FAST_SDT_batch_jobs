#!/bin/bash

# 2018/04/27
# NOTE: THE ORDER OF ORBIT PLOTS CATTED IS NOT NUMERICAL; IT IS BASED ON THE TIME OF CREATION (hence ls -lahtr)


dir=/SPENCEdata/software/sdt/batch_jobs/plots/20180426/kappa_fits/
NPLOTSTOCAT=100

# 2018/04/27
# PDFSUFF="_to300"
# startOrb=5683

# 2018/05/01
PDFSUFF="_to400"
startOrb=8217

if [ ! -d "${dir}" ]; then
    echo "Can't! N'existe pas: ${dir}";
    exit
fi

cd ${dir}
CWD=$(pwd);

mapfile -t myArray < <(ls -lAhtr --ignore='*pdf' 2>/dev/null | awk '{print $9}' | awk -v startOrb=${startOrb} -v nPlotsToCat=${NPLOTSTOCAT} 'BEGIN { orbDir="Orbit_" startOrb ; print orbDir; }; $0 ~ orbDir {for(i=1; i<=nPlotsToCat; i++) {getline; print}}')

kappArr=();
swayArr=();

for bro in ${myArray[@]}; do
    orb=${bro##Orbit_};
    printf "${orb}:";
    if [ -n "$(ls ${bro}/Kappa_summary* 2>/dev/null)" ]; then
	printf " K!"; kappArr=(${kappArr[@]} $(ls ${bro}/Kappa_summary*) )
    fi
    if [ -n "$(ls ${bro}/Strangeway_summary* 2>/dev/null)" ]; then
	printf " S!"; swayArr=(${swayArr[@]} $(ls ${bro}/Strangeway_summary*) )
    fi
    printf "\n"
    cd ${CWD}
done

if [ -e "sWayOrbits${PDFSUFF}.pdf" ]; then
    echo "About to overwrite sWayOrbits${PDFSUFF}.pdf--ain't gonna do it!"
    exit
fi

if [ -e "kappaOrbits${PDFSUFF}.pdf" ]; then
    echo "About to overwrite kappaOrbits${PDFSUFF}.pdf--ain't gonna do it!"
    exit
fi

gs -sDEVICE=pdfwrite -dEPSFitPage -dNOPAUSE -dBATCH -dSAFER -sOutputFile=sWayOrbits${PDFSUFF}.pdf ${swayArr[@]}
gs -sDEVICE=pdfwrite -dEPSFitPage -dNOPAUSE -dBATCH -dSAFER -sOutputFile=kappaOrbits${PDFSUFF}.pdf ${kappArr[@]}
