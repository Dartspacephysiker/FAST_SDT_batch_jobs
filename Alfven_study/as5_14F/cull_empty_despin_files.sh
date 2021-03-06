#!/bin/bash

CULLDIR='/SPENCEdata/software/sdt/batch_jobs/Alfven_study/as5_14F/batch_output__ucla_mag_despin'

echo "Culling contentless ucla_mag_despin, batch-outputted files..."
echo "Directory: ${CULLDIR}"

for file in ${CULLDIR}/Dartmouth_as5_dflux_*; do
    this="`wc -l ${file} | awk '{print $1}'`"
    [ ${this} -eq "42" ] && \
	[ "`ls -la ${file} | awk '{print $5}'`" -eq "2112" ] && \
	echo "Deleting ${file}..." && \
	rm ${file} -v && \
	echo "${file} ${this}" >> various_textfiles_and_notes/culled_ucla_mag_despin_files.txt 
done

echo "Bro!!"
