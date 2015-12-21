#!/usr/bin/awk -f

#2015/12/21
# awk_totaler.awk --- Are these time differences acceptable?

# Records are separated by newline characters
BEGIN { 
#    oldT             = $1      #Start time
#   oldL             = $0      #Start line

    totalNEvents     = 0
    totalNOrbits     = 0

    totalEvInMRDB    = 1154905
    totalEvInDB      = 1165350

}

{
    #Handle times
    totalNEvents     = totalNEvents + $10
    totalNOrbits     = totalNOrbits + 1
}

END {
    printf "-->\n"
    printf "-->Statistics of missing events for B-field mapping_ratios %s\n", ARGV[1]
    printf "=====================================================================\n\n"
    printf "Total missing N Events      : %i\n", totalNEvents
    printf "Total missing orbits        : %i\n", totalNOrbits
    printf "\n"
    printf "Total events in mapRatio DB : %i\n", totalEvInMRDB
    printf "Total events in DB          : %i\n", totalEvInDB

    nUnaccounted = totalEvInDB - (totalEvInMRDB + totalNEvents)
    if (  nUnaccounted == 0 )
    {
	printf "Great! The numbers make sense; just recover the missing %i events!\n",totalNEvents
    }
    else
    {
	printf "This doesn't add up. There are %i events not accounted for anywhere...\n", nUnaccounted
    }
}
