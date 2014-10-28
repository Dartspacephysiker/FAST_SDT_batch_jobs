#!/bin/bash

#Here's the script to 

#declare versions
#declare -a versarr=("1.3" "1.4" "1.5" "2.1" "2.2" "2.3" "2.4" "2.5" "2.6" "2.7" "2.8" "2.9")
declare -a versarr=("1.2.1.1" "1.2.1.2")

#cd ~/Desktop/MagCalHistory/versions/
cd /SPENCEdata/software/sdt/batch_jobs/Alfven_study/as5_14F/magcal_versions/MagCalHistory

#for magcal in MagDC.cal*; do
for vers in "${versarr[@]}"; do

    echo "Doing version ${vers}"
 
    #Use this to get version "vers" of a revised file
    cd SCCS; sccs get -r${v} -G../versions/MagDC.cal_v${v} s.MagDC.cal; cd -;
    
    cd ~/Desktop/MagCalHistory/versions/
    cp -f MagDC.cal_v${vers} /home/spencerh/software/sdt/Linux.2.6/lib/fast_fields_cals/MagDC.cal

    cd /SPENCEdata/software/sdt/batch_jobs/Alfven_study/as5_14F/magcal_versions/
    sed -i "s/version=.*/version='"${vers}"'/" alfven_stats_5_magcal_v.pro
    echo "Changed script:"
    sed -n '6p' alfven_stats_5_magcal_v.pro
    sdt_batch alfven.batch

done
