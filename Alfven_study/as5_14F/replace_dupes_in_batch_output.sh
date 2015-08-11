#!/bin/bash
shopt -s extglob

for i in {15000..16999..1}; do
    for j in {0..3..1}; do
	file="Dartmouth_as5_dflux_${i}_${j}below_aur_oval"
#	[ -e ${file} ] && bigfile=$(ls \"${file}--2015\*\" 2>/dev/null) && [ -e $bigfile ] && echo $bigfile
	[ -e ${file} ] && bigfile=$(find ./ -name '*${file}*' ) && echo $bigfile
    done
done

#for file in Dartmouth_as5_dflux_1[[:digit:]][[:digit:]][[:digit:]][[:digit:]]_{0,1,2}{,0,1,2,3,4,5,6,7,8,9}--*; do
#for file in Dartmouth_as5_dflux_1[[:digit:]][[:digit:]][[:digit:]][[:digit:]]_*--only_below_aur_oval; do
#for file in Dartmouth_as5_dflux_1[[:digit:]][[:digit:]][[:digit:]][[:digit:]]_*below_aur_oval; do
#    for bigfile in `echo \`ls ${file}--* 2>/dev/null\` `; do
#	[[ -e $bigfile ]] && echo "$file $bigfile"
#    done
#done
#
#For determining which is older
#for file in Dartmouth_as5_dflux_*_{0,1,2}{,0,1,2,3,4,5,6,7,8,9}; do
#    if [ \( -e "$file" \) -a \( -e "${file}below_aur_oval" \) ]
#    then 
#	if [ "$file" -ot "${file}below_aur_oval" ]
#	then echo "$file is older than below_aur_oval"
#	else echo "below_aur_val is older than $file"
#	fi
##    else echo "Couldn't find $file!"
#    fi
#done
