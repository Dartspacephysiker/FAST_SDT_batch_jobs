#!/bin/bash

#Here's the script to get an old MagDC cal version from SCCS, shove it in to SDT, then perform analysis for orbit 10000
#This should work just as well for any other orbit, or, in fact, for cals for any other mag data⸺we have the 
#SCCS files for all of the magnetometer data products.
#To change the orbit: edit alfven.batch
#To change which magnetometer data product: locate the desired cal under the 
#sdt directory ~/software/sdt/Linux.2.6/lib/fast_fields_cals/ and move this to "MagCalHistory".
#-->Get the revision history using `sccs prs [name of calfile].cal`, and find out which revisions exists to choose from

#declare versions
#declare -a versarr=("1.2.1.1" "1.2.1.2" "1.3" "1.4" "1.5" "2.1" "2.2" "2.3" "2.4" "2.5" "2.6" "2.7" "2.8" "2.9")
declare -a versarr=("2.5")

MAGCALHISTDIR='/SPENCEdata/software/sdt/batch_jobs/Alfven_study/as5_14F/magcal_versions/MagCalHistory'
#cd ~/Desktop/MagCalHistory/versions/
cd ${MAGCALHISTDIR}

#for magcal in MagDC.cal*; do
for vers in "${versarr[@]}"; do

    echo "Doing version ${vers}"
 
    #Use this to get version "vers" of a revised file
    cd SCCS 
    sccs get -r${vers} -G../versions/MagDC.cal_v${vers} s.MagDC.cal; 
    cd -
    
    cd ${MAGCALHISTDIR}/versions
    cp -f MagDC.cal_v${vers} /home/spencerh/software/sdt/Linux.2.6/lib/fast_fields_cals/MagDC.cal

    cd /SPENCEdata/software/sdt/batch_jobs/Alfven_study/as5_14F/magcal_versions/
    sed -i "s/version=.*/version='"${vers}"'/" alfven_stats_5_magcal_v.pro
    echo "Changed script:"
    sed -n '6p' alfven_stats_5_magcal_v.pro
    sdt_batch alfven.batch

done
