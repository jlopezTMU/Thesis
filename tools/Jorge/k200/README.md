
This is a description of the scripts involved in the process:

A) https://github.com/miranska/se_topics/blob/master/tools/Jorge/k200/CalcFK_200.R
 

INPUT-> all frequency files for all datasets for all periods
OUTPUT-> topX.csv file, which is a merge of all datasets for all periods for top X topics from 1..50 for N topics

B) https://github.com/miranska/se_topics/blob/master/tools/Jorge/k200/toFit_k200.R

INPUT-> topX.csv
OUTPUT-> this is the calculation of the nls and lm for the p powers we choose in this case p=6. So we obtain the coefficients of b here.

This is the script you asked me for.

C) https://github.com/miranska/se_topics/blob/master/tools/Jorge/k200/CalcFK_200.R

INPUT-> all frequency files for all datasets for all periods
OUTPUT-> FileFN-<x>-*

This program reads the frequencies files produced by the do_lda_analysis 
extracts top topics and calculates their corresponding F for X=x1,x2,.. and forms a file to plot those values.

D) https://github.com/miranska/se_topics/blob/master/tools/Jorge/k200/CalfKFFp_200.R

INPUT-> FileFN-*
OUTPUT-> File-toPlot-Fp-*

This program only generates the files to be plotted with K, F, Fp. It uses the coefficients calculated in B for b to obtain Fp. 
