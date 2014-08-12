
#!/bin/bash
for YEAR in 2012 2013 
do
	for MONTH in {1..12} 
	do
		 Rscript do_lda_analysis_topics.R ../../../topic_analysis/Posts.xml.w_ans.csv $YEAR $MONTH
	done
done
