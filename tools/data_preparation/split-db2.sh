!/bin/sh
## usage bash split-db2.sh inFile > outFile
awk -F"\t" '{split($3,a,/\^&\^/);for(t in a)if(a[t]=="db2"){print;next}}' $1
