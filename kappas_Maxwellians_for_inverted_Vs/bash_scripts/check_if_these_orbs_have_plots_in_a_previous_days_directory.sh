#!/bin/bash

orbitList=/SPENCEdata/software/sdt/batch_jobs/kappas_Maxwellians_for_inverted_Vs/orbits__toinspect_20180430.txt
inspectDir=/SPENCEdata/software/sdt/batch_jobs/plots/20180426/kappa_fits/
checkForPrevDir=/SPENCEdata/software/sdt/batch_jobs/plots/20180425/kappa_fits/

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

if [ ! -d "${checkForPrevDir}" ]; then
    echo "checkForPrevDir doesn't exist: ${checkForPrevDir}"
    echo "Out!"
    exit
fi

mapfile -t myArray < ${orbitList}

echo "Orbs: ${myArray[@]}"

cd ${inspectDir}

CWD=$(pwd)
for bro in ${myArray[@]}; do
    tmpDir=Orbit_${bro}
    nToSkip=0

    if [ -d "${tmpDir}" ]; then
	# echo ""
	# printf "Have ${tmpDir} ... "
	if [ -d "${checkForPrevDir}/${tmpDir}/" ]; then
	    # printf "Also have ${tmpDir} in ${checkForPrevDir}!\n"

	cd ${checkForPrevDir}/${tmpDir}/
	# ls -1 Kappa_summary* KapSumTmp* 2>/dev/null
	nToSkip=$(ls -1 Kappa_summary* KapSumTmp* 2>/dev/null | wc -l)
	# echo "${bro}: ${nFil}"
	# killFile=$(ls -t1r Kappa_summary* 2>/dev/null | head -n 1)
	# if [ ${nFil} -eq 0 ]; then
	#     echo "No file!"
	# fi
	# if [ ${nFil} -ge 1 ]; then
	#     echo "rm ${killFile}"
	#     # rm -v ${killFile}
	# fi
	# if [ ${nFil} -gt 1 ]; then
	#     echo "2 files!"
	#     keepFiles=$(ls -t1r Kappa_summary* | tail -n $((${nFil}-1)))

	#     echo "keepFiles: "
	#     for keeper in ${keepFiles[@]}; do

	# 	echo "mv ${keeper} KapSumTmp${keeper##Kappa_summary}"
	#         # mv -v ${keeper} KapSumTmp${keeper##Kappa_summary}

	#     done
	# fi
	# else
	#     printf "\n"
	fi

    fi

    echo "${bro} ${nToSkip}"

    cd ${CWD}
done


