#!/bin/sh
## usage bash split-nondb2.sh inFile > outFile
awk -F"\t" '{
             is_db2_present=0
             split($3,tag,/\^&\^/);
             for(t in tag)
                if(tag[t]=="db2")
                  is_db2_present=1;
             if(is_db2_present==0)  
                print;
             }' $1
