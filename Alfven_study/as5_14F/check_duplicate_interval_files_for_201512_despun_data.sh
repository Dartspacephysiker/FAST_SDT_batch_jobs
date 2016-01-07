#!/bin/bash

firstOrb=500
lastOrb=5000

#filePref=Dartmouth_as5_dflux_2138_0----below_aur_oval--ucla_mag_despin

outDir="/SPENCEdata/software/sdt/batch_jobs/Alfven_study/as5_14F/batch_output__ucla_mag_despin_201512/"

filePref="Dartmouth_as5_dflux_"
fileSuff="----below_aur_oval--ucla_mag_despin"


for (( i=firstOrb ; i < lastOrb ; i++ )) 
do 
    file="${outDir}${filePref}${i}_0${fileSuff}"

#    [ -e ${file} ] && echo ${file}
    if [ -e ${file} ] 
    then
#	echo ${i}
	file_next="${outDir}${filePref}${i}_1${fileSuff}"
	if [ -e ${file_next} ]
	then
	    echo ${i}
#	    diff -qs ${file} ${file_next} | grep -v differ
	    cmp --silent ${file} ${file_next} && echo ${i} && echo "0 1"
	fi
    fi
done


