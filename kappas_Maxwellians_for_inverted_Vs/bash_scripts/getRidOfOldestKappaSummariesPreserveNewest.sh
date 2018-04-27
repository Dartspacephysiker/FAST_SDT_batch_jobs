#!/bin/bash

orbitList=/SPENCEdata/software/sdt/batch_jobs/kappas_Maxwellians_for_inverted_Vs/orbits.txt
inspectDir=/SPENCEdata/software/sdt/batch_jobs/plots/20180425/kappa_fits/

if [ ! -e "${orbitList}" ]; then
    echo "Doesn't exist: ${orbitList}"
    echo "Out!"
    exit
fi

if [ ! -d "${inspectDir}" ]; then
    echo "Doesn't exist: ${inspectDir}"
    echo "Out!"
    exit
fi

mapfile -t myArray < ${orbitList}

cd ${inspectDir}

CWD=$(pwd)
for bro in ${myArray[@]}; do
    tmpDir=Orbit_${bro}
    if [ -d "${tmpDir}" ]; then
	cd ${tmpDir}
	nFil=$(ls -1 Kappa_summary* 2>/dev/null | wc -l)
	echo "Orb ${bro}: ${nFil} kappaSums"
	killFile=$(ls -t1r Kappa_summary* 2>/dev/null | head -n 1)
	if [ ${nFil} -eq 0 ]; then
	    echo "No file!"
	fi
	if [ ${nFil} -ge 1 ]; then
	    echo "rm ${killFile}"
	    # rm -v ${killFile}
	fi
	if [ ${nFil} -gt 1 ]; then
	    echo "2 files!"
	    keepFiles=$(ls -t1r Kappa_summary* | tail -n $((${nFil}-1)))

	    echo "keepFiles: "
	    for keeper in ${keepFiles[@]}; do

		echo "mv ${keeper} KapSumTmp${keeper##Kappa_summary}"
	        # mv -v ${keeper} KapSumTmp${keeper##Kappa_summary}

	    done
	fi
    fi
    cd ${CWD}
done


