
#!/bin/bash
for YEAR in 2012 2013 2014
do
	for QUARTER in {1..4} 
	do
		echo "Running script for $YEAR-$QUARTER"
		 Rscript do_lda_analysis_topics_quarterly.R /media/data/text/joomla/Posts.xml.w_ans.csv $YEAR $QUARTER
		echo "Done with $YEAR-$QUARTER"
	done
done
