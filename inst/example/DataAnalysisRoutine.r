#This example perform the estimation of fuzzy measure from data
#Compute Shapley value and Interaction Index.
#Here, we only demonstrate the computation of those values on a data set Business-Asia.txt.
#Other data set can be applied similarly.
library("Rfmtool")

#load data from files.
busiasiadata <- as.matrix(read.table("../data/Preprocessed/Business-Asia.txt"))

#estimate fuzzy measure from imperical data sets.
estimatedfuzzy <- fm.fitting(busiasiadata);

#the estimate fuzzy measures are in Mobius representation.
#we tranform them to general representation here.
generalfuzzy  <- fm.Zeta(estimatedfuzzy);

#compute Shapley value and Interaction Index
ShapleyVal <- fm.Shapley(generalfuzzy);
InteracVal <- fm.Interaction(estimatedfuzzy);


