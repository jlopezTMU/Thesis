
#!/bin/bash
for YEAR in 2012 2013 2014 
do
	for MONTH in {1..12} 
	do
		echo "Running script for $YEAR-$MONTH"
		 Rscript do_lda_analysis_topics.R /media/data/text/topic_analysis/Posts.xml.w_ans.csv $YEAR $MONTH
		echo "Done with $YEAR-$MONTH"
	done
done
