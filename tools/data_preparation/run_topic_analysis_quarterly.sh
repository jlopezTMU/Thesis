
#!/bin/bash
for YEAR in 2012 2013 
do
	for QUARTER in {1..4} 
	do
		 Rscript do_lda_analysis_topics_quarterly.R ../../../topic_analysis/Posts.xml.w_ans.csv $YEAR $QUARTER
	done
done
