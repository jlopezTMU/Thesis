library(topicmodels)
data("AssociatedPress", package = "topicmodels")
topicCount = 3

mdl <- LDA(AssociatedPress[1:10,1:1300], topicCount) #LDA model

terms(mdl, 5) #show top 5 terms

mdl.post <- posterior(mdl) #get posterior data

mdl.post$terms[[1,"bloomberg"]] #get the p-value  for term "bloomberg" in the 1st topic
mdl.post$terms[[2,"bloomberg"]] #get the p-value  for term "bloomberg" in the 2nd topic

# you can return terms with p value gtreater than a certain value, but it is hard to 
# find a good constant value
terms(mdl, threshold = 0.01) #return terms with p>0.01

#here is the way to automate the process:

top.terms <- terms(mdl, 5) #show top 5 terms
#print out top1 term for 1st topic
cat(top.terms[1,1], mdl.post$terms[[1, top.terms[1,1]]])
#print out top2 term for 1st topic 
cat(top.terms[2,1], mdl.post$terms[[1, top.terms[2,1]]])
#print out top1 term for 2nd topic
cat(top.terms[1,2], mdl.post$terms[[2, top.terms[1,2]]])
#etc.
