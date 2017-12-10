library("Rfmtool")


#function to compute (MAE).
mae <- function(obs, pred) mean(abs(obs-pred))


#function to evaluate a given data set using 10-fold cross validation
#Mean Absolute Error (MAE) is used to measure the performance.

evalfunc <- function(datafile)
{
data <- as.matrix(read.table(datafile));
size <- dim(as.matrix(data));
row <- size[1];
col <- size[2];
inputdim <- col - 1;


#evaluate the Choquet integral using 10-fold cross validation,
k <- 10;
id <- sample(rep(seq_len(k), length.out=nrow(data)));

kadderror <- array(0,c(1,inputdim));

for (kadd in seq_len(inputdim))
{
	maeVal <- array(0,c(1,k));	
	for (i in seq_len(k))
	{
		test_matrix <- data [id==i, ];
    	      train_matrix <- data [id!=i, ];
	
		#estimate fuzzy measure from training data.
		cat("estimating fuzzy measure for the", i, "th iternation for the", kadd,"-additive...\n");
		estfuzzy <- fm.fitting(train_matrix,kadd);
	
		predicVal <- array(0,c(1,nrow(test_matrix)));
		originVal <- array(0,c(1,nrow(test_matrix)));

		count <- 1;
		for (f in seq_len(nrow(test_matrix)))
		{
			eachrec <- test_matrix[f,];
			#compute Choquet integral for each testing input 
			#with fuzzy measure in Mobius representation.
			ChoVal <- fm.ChoquetMob(eachrec[1:col-1],estfuzzy);
		
			predicVal[count] <- ChoVal;
			originVal[count] <- eachrec[col];
			count <- count +1;
		}
		maeVal[i] <- mae(originVal,predicVal);
	}
	kadderror[kadd] <- mean(maeVal);
}
return(kadderror);
}


#The main evaluation routine is here.

#load data from files.
#perform 10-fold cross validation for different data sets with k-additive.
#k can be set to values from 1 to n continuously.

busidata <- "../data/Preprocessed/Business.txt";
busierror <- evalfunc(busidata);

coupdata <- "../data/Preprocessed/Couple.txt";
couperror <- evalfunc(coupdata);

famidata <- "../data/Preprocessed/Family.txt";
famierror <- evalfunc(famidata);

#Note that the estimated MAE can vary slightly between different runs 
#due to the randomness of cross validation procedure.




