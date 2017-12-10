library("Rfmtool")


#function to compute (MAE).
mae <- function(obs, pred) mean(abs(obs-pred))


#Wrapper function to estimate OWA weights from data.

fm.fittingOWA<- function(data)
{
	size <- dim(as.matrix(data));
	n <- size[2] - 1;
	datanum <- size[1];
	WeightVal <- array(0,n);
 
	WeightValue <- .C("fittingOWACall", as.integer(n),
	  				    as.integer(datanum),
			              out = as.numeric(WeightVal),
			                    as.numeric(t(data)));
					
	return (WeightValue$out);
}

#Wrapper function to comput OWA value for a given input x and estimated weight v.
fm.OWA <- function(x,v)
{
	OWAVal <- -1; #this is just a initial value.
	OWAValue <- .C("OWACall", as.numeric(x),
				  as.numeric(v),
				  as.integer(length(x)),
			    out = as.numeric(OWAVal));
        return (OWAValue$out);
}



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

maeVal <- array(0,c(1,k));	
for (i in seq_len(k))
{
	test_matrix <- data [id==i, ];
      train_matrix <- data [id!=i, ];
	
	#estimate OWA weights from training data.
	cat("estimating OWA weights for the", i, "th iternation...\n");
	estweight <- fm.fittingOWA(train_matrix);
	
	predicVal <- array(0,c(1,nrow(test_matrix)));
	originVal <- array(0,c(1,nrow(test_matrix)));

	count <- 1;
	for (f in seq_len(nrow(test_matrix)))
	{
		eachrec <- test_matrix[f,];
		#compute OWA value
		OWAVal <- fm.OWA(eachrec[1:col-1],estweight);
		
		predicVal[count] <- OWAVal ;
		originVal[count] <- eachrec[col];
		count <- count +1;
	}
	maeVal[i] <- mae(originVal,predicVal);
}

return(mean(maeVal));
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




