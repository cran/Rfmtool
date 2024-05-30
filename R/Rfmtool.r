# Rfmtool Package v5.0

require("Rcpp")

fm <- function()
{
    # This function outputs a list of all functions included in this toolbox.
    
    print("The list of functions in Rfmtool Tool Box:")

    print("fm.Init <- function([number of variables]) - returns [environment]")
        print("fm.PrepareSparseFM <- function([number of variables], [tuples to be added], [cardinalities and indices of tuples elements]) - returns [environment (sparse representation)]")
        print("fm.add_pair_sparse([Indices (1-based)], [Indices (1-based)], [value to add], [environment (sparse representation)])")
        print("fm.add_singletons_sparse([vector of singletons of size n], [environment (sparse representation)])")
        print("fm.add_tuple_sparse([nonzero tuples cardinalities], [tuple value to be added], [environment (sparse representation)])")
        print("fm.Banzhaf([fuzzy measure (general represenation)], [environment])")
        print("fm.Banzhaf2addMob([number of variables], [fuzzy measure (mobius represenation)])")
        print("fm.BanzhafMob_sparse([number of variables], [environment (sparse representation)])")
        print("fm.BanzhafMob([fuzzy measure (mobius represenation)], [environment])")
        print("fm.Bipartition([fuzzy measure (general represenation)], [environment])")
        print("fm.BipartitionBanzhaf([fuzzy measure (general represenation)], [environment])")
        print("fm.check_convexity_monotonicity_mob([fuzzy measure (general represenation)], [fuzzy measure (mobius represenation) length], [environment])")
        print("fm.check_monotonicity_mob_2additive([fuzzy measure (general represenation)], [number of variables], [Auxiliary array of length n^2])")
        print("fm.check_monotonicity_mob([fuzzy measure (general represenation)], [fuzzy measure (mobius represenation) length], [environment])")
        print("fm.check_monotonicity_sort_insert([fuzzy measure (general represenation)], [indices], [environment])")
        print("fm.check_monotonicity_sort_merge([fuzzy measure (general represenation)], [indices], [environment])")
        print("fm.check_monotonicity([fuzzy measure (general represenation)], [environment])")
        print("fm.Choquet([input criteria], [fuzzy measure (general represenation)],[environment])")
        print("fm.Choquet2addMob([number of variables], [input criteria], [fuzzy measure (mobius represenation)])")
        print("fm.ChoquetCoMobKInter([input criteria], [fuzzy measure (mobius represenation)], [k-additive], [environment])")
        print("fm.ChoquetKinter([input criteria], [fuzzy measure (general represenation)], [k-interactivity parameter], [environment])")
        print("fm.ChoquetMob_sparse([input criteria],[environment (sparse representation)])")
        print("fm.ChoquetMob([input criteria], [fuzzy measure (mobius represenation)],[environment])")
        print("fm.ConstructLambdaMeasure([singletons (array of size n of the values of fuzzy measure at singletons)],[environment])")
        print("fm.ConstructLambdaMeasureMob([singletons (array of size n of the values of fuzzy measure at singletons)],[environment])")
        print("fm.ConvertCoMob2Kinte([fuzzy measure (mobius represenation)], [k-additive], [Integer flag], [environment])")
        print("fm.dualm([fuzzy measure (general represenation)], [environment])")
        print("fm.dualmMob([fuzzy measure (mobius represenation)] ,[environment])")
        print("fm.dualMobKadd([fuzzy measure (mobius represenation)],[ environment], [k-additive])")
        print("fm.EntropyChoquet([fuzzy measure (general represenation)],[environment])")
        print("fm.EntropyChoquetMob([fuzzy measure (mobius represenation)])")
        print("fm.expand_2add_full([number of variables], [environment (sparse representation)])")
        print("fm.expand_sparse_full([number of variables], [environment (sparse representation)])")
        print("fm.export_maximal_chains([fuzzy measure (general represenation)], [environment])")
        print("fm.fitting([emprical data], [k-additive])")
        print("fm.fitting2additive([emprical data],[other options])")
        print("fm.fittingKinteractive([emprical data], [k-interactive], [K])")
        print("fm.fittingKinteractiveAuto([emprical data], [k-interactive])")
        print("fm.fittingKinteractiveMarginal([emprical data], [k-interactive], [K])")
        print("fm.fittingKinteractiveMarginalMC([emprical data], [k-interactive], [K])")
        print("fm.fittingKinteractiveMC([emprical data], [k-interactive], [K])")
        print("fm.fittingKmaxitive([emprical data], [environment], [k-additive])")
        print("fm.fittingKtolerant([emprical data], [environment], [k-additive])")
        print("fm.fittingMob([emprical data], [environment], [k-additive])")
        print("fm.fittingOWA([emprical data], [environment])")
        print("fm.fittingWAM([emprical data], [environment])")
        print("fm.fm_arraysize([environment], [k-interactivity parameter])")
        print("fm.generate_fm_randomwalk([num fuzzy measures], [number of variables], [k-additive], [other options], [environment])")
        print("fm.FreeSparseFM<- function([environment (sparse representation)]) -  returns Frees the memory previously allocated in envsp")
        print("fm.FuzzyMeasureFitLP <- function([emprical data], [k-additive], [other options]) - returns standard fuzzy measure")
        print("fm.FuzzyMeasureFitLPMob <- function([emprical data], [k-additive], [other options]) - returns fuzzy measure (mobius represenation)")
        print("fm.generate_antibuoyant([environment])")
        print("fm.generate_balanced([num fuzzy measures], [environment])")
        print("fm.generate_fm_2additive_concave([num fuzzy measures], [number of variables])")
        print("fm.generate_fm_2additive_convex_sparse([number of variables], [environment (sparse representation)])")
        print("fm.generate_fm_2additive_convex_withsomeindependent([num fuzzy measures], [number of variables])")
        print("fm.generate_fm_2additive_convex([num fuzzy measures], [number of variables])")
        print("fm.generate_fm_2additive_randomwalk2([num fuzzy measures], [number of variables], [other options])")
        print("fm.generate_fm_2additive([num fuzzy measures], [number of variables])")
        print("fm.generate_fm_kadditive_convex_sparse([number of variables], [k-additive], [structure values [environment (sparse representation)])")
        print("fm.generate_fm_kinteractivedualconcave([num fuzzy measures], [number of variables], [k-additive], [other options], [environment])")
        print("fm.generate_fm_kinteractivedualconvex([num fuzzy measures], [number of variables], [k-additive], [other options], [environment])")
        print("fm.generate_fm_minplus([num fuzzy measures], [k-interactive], [other options], [environment])")
        print("fm.generate_fm_sorting([num fuzzy measures], [other options], [environment])")
        print("fm.generate_fm_tsort([num fuzzy measures], [k-interactive], [other options], [environment])")
        print("fm.generate_fmconvex_tsort([num fuzzy measures], [k-interactive], [other options], [environment])")
        print("fm.get_num_tuples([environment (sparse representation)])")
        print("fm.get_sizearray_tuples([environment (sparse representation)])")
        print("fm.Interaction([standard fuzzy measure],[environment])")
        print("fm.InteractionMob([fuzzy measure (mobius represenation)],[environment])")
        print("fm.InteractionB([standard fuzzy measure],[environment])")
        print("fm.InteractionBMob([fuzzy measure (mobius represenation)],[environment])")
        print("fm.is_inset_sparse([tuple indexed], [tuple cardinality], [Element (1-based)], [environment (sparse representation)])")
        print("fm.is_subset_sparse([tuple indexed], [tuple cardinality], [tuple indexed], [tuple cardinality], [environment (sparse representation)])")
        print("fm.IsMeasureAdditive([fuzzy measure (general represenation)],[environment])")
        print("fm.IsMeasureAdditiveMob([fuzzy measure (mobius represenation),[environment]])")
        print("fm.IsMeasureBalanced([fuzzy measure (general represenation)],[environment]])")
        print("fm.IsMeasureBalancedMob([fuzzy measure (mobius represenation)],[environment])")
        print("fm.IsMeasureKmaxitive([fuzzy measure (general represenation)],[environment])")
        print("fm.IsMeasureKmaxitiveMob([fuzzy measure (mobius represenation)],[environment])")
        print("fm.IsMeasureSelfdual([fuzzy measure (general represenation)],[environment])")
        print("fm.IsMeasureSelfdualMob([fuzzy measure (mobius represenation)],[environment])")
        print("fm.IsMeasureSubadditive([fuzzy measure (general represenation)],[environment])")
        print("fm.IsMeasureSubadditiveMob([fuzzy measure (mobius represenation)],[environment])")
        print("fm.IsMeasureSubmodular([fuzzy measure (general represenation)],[environment])")
        print("fm.IsMeasureSubmodularMob([fuzzy measure (mobius represenation)],[environment])")
        print("fm.IsMeasureSuperadditive([fuzzy measure (general represenation)],[environment])")
        print("fm.IsMeasureSuperadditiveMob([fuzzy measure (mobius represenation)],[environment])")
        print("fm.IsMeasureSupermodular([fuzzy measure (general represenation)],[environment])")
        print("fm.IsMeasureSupermodularMob([fuzzy measure (mobius represenation)],[environment])")
        print("fm.IsMeasureSymmetric([fuzzy measure (general represenation)],[environment])")
        print("fm.IsMeasureSymmetricMob([fuzzy measure (mobius represenation)],[environment])")
        print("fm.max_subset_sparse(input criteria], [tuple indexed indices], [tuple cardinality], [environment (sparse representation)])")
        print("fm.min_subset_sparse(input criteria], [tuple indexed indices], [tuple cardinality], [environment (sparse representation)])")
        print("fm.Mobius([fuzzy measure (general represenation)],[environment])")
        print("fm.NonadditivityIndex([fuzzy measure (general represenation)], [environment])")
        print("fm.NonadditivityIndexMob([fuzzy measure (mobius represenation)], [environment])")
        print("fm.NonmodularityIndex_sparse([number of variables], [environment (sparse representation)])")
        print("fm.NonmodularityIndex([fuzzy measure (general represenation)], [environment])")
        print("fm.NonmodularityIndexKinteractive([fuzzy measure (general represenation)], [environment], [k-additive])")
        print("fm.NonmodularityIndexMob([fuzzy measure (mobius represenation)], [environment])")
        print("fm.NonmodularityIndexMobkadditive([fuzzy measure (mobius represenation)], [environment], [k-additive])")
        print("fm.OrnessChoquet([fuzzy measure (standard represenation)],[environment])")
        print("fm.OrnessChoquetMob([fuzzy measure (mobius represenation)],[environment])")
        print("fm.populate_fm_2add_sparse_from2add([number of variables], [fuzzy measure (general represenation)], [environment (sparse representation)])")
        print("fm.populate_fm_2add_sparse([singletons (array of size n of the values of fuzzy measure at singletons)], [size numpairs], [array 0-based], [size numpairs indices], [size numpairs indices], [environment (sparse representation)])")
        print("fm.Shapley([fuzzy measure (general represenation)], [environment])")
        print("fm.Shapley2addMob([number of variables], [fuzzy measure (mobius represenation)])")
        print("fm.ShapleyMob_sparse([number of variables], [environment (sparse representation)])")
        print("fm.ShapleyMob([fuzzy measure (mobius represenation)],[environment])")
        print("fm.ShowCoalitions([environment])")
        print("fm.ShowCoalitionsCard([environment])")
        print("fm.sparse_get_pairs([environment (sparse representation)])")
        print("fm.sparse_get_singletons([environment (sparse representation)])")
        print("fm.sparse_get_tuples([environment (sparse representation)])")
        print("fm.Sugeno([input criteria], [fuzzy measure (general represenation)],[environment])")
        print("fm.SugenoMob([input criteria], [fuzzy measure (mobius represenation)],[environment])")
        print("fm.test()")
        print("fm.tuple_cardinality_sparse([in the list of tuples], [environment (sparse representation)])")
        print("fm.Zeta([fuzzy measure (mobius represenation)],[environment])")
}


fm.PrepareSparseFM<- function(n, tups=NULL, tupsidx=NULL)
{
  if(is.null(tups)) tups<-vector();
  if(is.null(tupsidx)) tupsidx<-vector(); 
  
  if(n<=1)return(NULL);
  tupsz=length(tups);
  tupszidx=length(tupsidx);
  tupidxsz=tupsz*n;

  out<- .C("Prepare_FM_sparseCall",n=as.integer(n), as.integer(tupszidx), as.double(tups), as.integer(tupsidx), 
  singletons=double(n), pairs1=double(n), tuples=double(n), pairsidx=integer(2*n), 
  tuplesidx=integer(2*n), tuplescon=integer((tupsz+1)*(n+2)), dims=integer(4) );

  envsp=list();
  envsp$n=n
  envsp$singletons=out$singletons
  envsp$pairs=out$pairs1
  envsp$tuples=out$tuples
  envsp$pairsidx=out$pairsidx
  envsp$tuplesidx=out$tuplesidx
  envsp$tuplescon=out$tuplescon
  envsp$dims=out$dims  
  
 return(envsp); 
#   envsp <- .Call("create");
#    t <- 0;
#print(envsp);
#    out <- .Call("Prepare_FM_sparseCall", n = as.integer(n), t = as.integer(t), tuples = as.integer(1:1), envsp);
#    return(envsp);
}

fm.FreeSparseFM<- function(envsp)
{
    envsp <- NULL;
	return(envsp);
  #  gc();
}

fm.Init <- function(n1)
{

    n<-as.integer(n1);
    m1 <-2^n;

    out<-.C("Preparations_FMCall",n=as.integer(n), m=as.integer(m1), card=as.integer(1:m1),cardpos=as.integer(1:(n+1)),
	bit2card=as.double(1:m1),card2bit=as.double(1:m1), factorials=as.double(1:(n+1))
	#bit2card=as.integer(1:m1),card2bit=as.integer(1:m1), factorials=as.double(1:(n+1))

 );
 
					
    return (out);
}


fm.errorcheck <- function(env=NULL)
{
	if(is.null(env)) return(TRUE);
	if(env$n!=log2(env$m)) return(TRUE);
	if((env$n+1) !=length(env$cardpos)) return(TRUE);
	if(env$m!=length(env$card)) return(TRUE);
	if(env$m!=length(env$card2bit)) return(TRUE);
	if(env$m!=length(env$bit2card)) return(TRUE);

	if((env$n+1)!=length(env$factorials)) return(TRUE);

	return(FALSE);
}

fm.Free<-function(env=NULL)
{
    if(fm.errorcheck(env)) {
        print("Incorrect environment specified, call env<-fm.Init(n) first before freeing.");
        return (NULL);
    }
    env$card<-vector()
    env$cardpos<-vector()
    env$bit2card<-vector()
    env$card2bit<-vector()
    env$factorials<-vector()
    env$n<-0
    # env<-fm.Free(env)
}


fm.errorchecksparse <- function(envsp)
{
	if(is.null(envsp)) return(TRUE);
	if(is.null(envsp$singletons) | is.null(envsp$pairs) | is.null(envsp$tuples) | is.null(envsp$tuplesidx) | is.null(envsp$pairsidx)| is.null(envsp$tuplescon)| is.null(envsp$dims)) return(TRUE);
    if(envsp$n>length(envsp$singletons))return(TRUE);
    if(envsp$dim[1]>length(envsp$pairs))return(TRUE);
    if(envsp$dim[2]>length(envsp$tuples))return(TRUE);	
	
	return(FALSE);
}

fm.Banzhaf <- function(v,env=NULL)
{
    # Calculates an array of Banzhaf indices
	if(fm.errorcheck(env)) {
		print("Incorrect environment specified, call env<-fm.Init(n) first.");
		return (NULL);
	}

    BanzhafVal <- array(0.0,log2(length(v)));

	if(env$m!=length(v)) {
		print("The environment mismatches the dimension to the fuzzy measure.");
		return (NULL);
	}


	BanzhafValue <- .C("BanzhafCall", as.numeric(v), 
        out = as.numeric(BanzhafVal),
        as.integer(log2(length(v))), 
	 as.integer(env$m), as.integer(env$card), as.integer(env$cardpos),as.double(env$bit2card),as.double(env$card2bit),as.double(env$factorials)
);
					
    return (round(BanzhafValue$out, digits=4));
}


fm.BanzhafMob <- function(Mob,env=NULL)
{
	if(fm.errorcheck(env)) {
		print("Incorrect environment specified, call env<-fm.Init(n) first.");
		return (NULL);
	}
	if(env$m!=length(Mob)) {
		print("The environment mismatches the dimension to the fuzzy measure.");
		return (NULL);
	}

    # Calculates an array of Banzhaf indices for Mobius fuzzy measure

    BanzhafMobVal <- array(0.0,log2(length(Mob)));
	BanzhafMobValue <- .C("BanzhafMobCall", as.numeric(Mob), 
        out = as.numeric(BanzhafMobVal),
        as.integer(log2(length(Mob))), 
	 as.integer(env$m), as.integer(env$card), as.integer(env$cardpos),as.double(env$bit2card),as.double(env$card2bit),as.double(env$factorials)
);
					
    return (round(BanzhafMobValue$out, digits=4));
}


fm.Choquet <- function(x, v,env=NULL)
{
    # Calculates the value of a discrete Choquet integral of the input x, with provided fuzzy measure v (in general representation)
	if(fm.errorcheck(env)) {
		print("Incorrect environment specified, call env<-fm.Init(n) first.");
		return (NULL);
	}	
	if(env$m!=length(v)|| env$n!=length(x)) {
		print("The environment mismatches the dimension to the fuzzy measure.");
		return (NULL);
	}

    ChoquetVal <- -1; #this is just a initial value.
    ChoquetValue <- .C("ChoquetCall", as.numeric(x),
        as.numeric(v),
        as.integer(length(x)),
        out = as.numeric(ChoquetVal), 
	 as.integer(env$m), as.integer(env$card), as.integer(env$cardpos),as.double(env$bit2card),as.double(env$card2bit),as.double(env$factorials)
);
    return (ChoquetValue$out);
}


fm.ChoquetMob <- function(x, Mob,env=NULL)
{
	if(fm.errorcheck(env)) {
		print("Incorrect environment specified, call env<-fm.Init(n) first.");
		return (NULL);
	}
	if(env$m!=length(Mob)||env$n!=length(x)) {
		print("The environment mismatches the dimension to the fuzzy measure.");
		return (NULL);
	}


    ChoquetVal <- -1; #this is just a initial value.
	ChoquetMobValue <- .C("ChoquetMobCall", as.numeric(x),
					        as.numeric(Mob),
				 	        as.integer(length(x)),
			                  out = as.numeric(ChoquetVal), 
	 as.integer(env$m), as.integer(env$card), as.integer(env$cardpos),as.double(env$bit2card),as.double(env$card2bit),as.double(env$factorials)
);
	return (ChoquetMobValue$out);
}

fm.ChoquetKinter <- function(x, v, kint, env=NULL)
{
	if(fm.errorcheck(env)) {
		print("Incorrect environment specified, call env<-fm.Init(n) first.");
		return (NULL);
	}
	if(env$m!=length(v)||env$n!=length(x)) {
		print("The environment mismatches the dimension to the fuzzy measure.");
		return (NULL);
	}


    ChoquetVal <- -1; #this is just a initial value.
	ChoquetMobValue <- .C("ChoquetkinterCall", as.numeric(x),
					        as.numeric(v),
				 	        as.integer(length(x)),
			                  out = as.numeric(ChoquetVal), as.integer(kint),
	 as.integer(env$m), as.integer(env$card), as.integer(env$cardpos),as.double(env$bit2card),as.double(env$card2bit),as.double(env$factorials)
);
	return (ChoquetMobValue$out);
}

	
	
	

fm.ConstructLambdaMeasure <- function(singletons,env=NULL)
{
	# Finds the value of lambda and calculates the rest of the values of the fuzzy measure,
    # given its values at singletons. singletons is an array of size n.
    # lambda and v are the outputs, v is in standard representation and binary ordering 
    # (array v of size m should be allocated by the calling routine).
	if(fm.errorcheck(env)) {
		print("Incorrect environment specified, call env<-fm.Init(n) first.");
		return (NULL);
	}	
	if(env$n!=length(singletons)) {
		print("The environment mismatches the dimension to the fuzzy measure.");
		return (NULL);
	}

    lambda <- array(-1, 1);      # initial value of lambda: array of length 1 with value -1
    v <- array(0.0, 2^length(singletons));   # array of m zeros
	ConstructLambdaMeasureValue <- .C("ConstructLambdaMeasureCall", 
        as.numeric(singletons),
        out1 = as.numeric(lambda),
        out2 = as.numeric(v),
        as.integer(length(singletons)), 
	 as.integer(env$m), as.integer(env$card), as.integer(env$cardpos),as.double(env$bit2card),as.double(env$card2bit),as.double(env$factorials)

    );

    return(list(lambda=ConstructLambdaMeasureValue$out1, measure=ConstructLambdaMeasureValue$out2));
}


fm.ConstructLambdaMeasureMob <- function(singletons,env=NULL)
{
	# Finds the value of lambda and calculates the rest of the values of the fuzzy measure,
    # given its values at singletons. singletons is an array of size n.
    # lambda and Mob are the outputs, Mob is in standard representation and binary ordering 
    # (array Mob of size m should be allocated by the calling routine).
	if(fm.errorcheck(env)) {
		print("Incorrect environment specified, call env<-fm.Init(n) first.");
		return (NULL);
	}	
	if(env$n!=length(singletons)) {
		print("The environment mismatches the dimension to the fuzzy measure.");
		return (NULL);
	}

	out<-fm.ConstructLambdaMeasure(singletons,env);

	out$measure<-fm.Mobius(out$measure,env);

    return(out);
}


fm.dualm <- function(v,env=NULL)
{
    # Calculates the dual of fuzzy measure v, returns it as value of the function (array of size m).
	if(fm.errorcheck(env)) {
		print("Incorrect environment specified, call env<-fm.Init(n) first.");
		return (NULL);
	}
	if(env$m!=length(v)) {
		print("The environment mismatches the dimension to the fuzzy measure.");
		return (NULL);
	}

    dualmVal <- array(0.0,length(v));  # array of m zeros
    dualmValue <- .C("dualmCall", 
        as.numeric(v),
        out = as.numeric(dualmVal),
        #as.integer(log2(length(v))),
        as.integer(length(v)), 
	 as.integer(env$m), as.integer(env$card), as.integer(env$cardpos),as.double(env$bit2card),as.double(env$card2bit),as.double(env$factorials)

    );
    return (dualmValue$out);
}


fm.dualmMob <- function(Mob,env=NULL)
{
    # Calculates the dual of fuzzy measure Mob in Mobius representation, returns it as value of the function (array of size m).
	if(fm.errorcheck(env)) {
		print("Incorrect environment specified, call env<-fm.Init(n) first.");
		return (NULL);
	}

	if(env$m!=length(Mob)) {
		print("The environment mismatches the dimension to the fuzzy measure.");
		return (NULL);
	}

     dualmVal <- array(0.0,length(Mob));  # array of m zeros
    dualmValue <- .C("dualMobCall", 
        as.numeric(Mob),
        out = as.numeric(dualmVal),
        #as.integer(log2(length(Mob))),
        as.integer(length(Mob)), 
	 as.integer(env$m), as.integer(env$card), as.integer(env$cardpos),as.double(env$bit2card),as.double(env$card2bit),as.double(env$factorials)

    );

    return (dualmValue$out);
}


fm.EntropyChoquet <- function(v,env=NULL)
{
    # Calculates entropy value of the Choquet integral of fuzzy measure in general representation. 
	if(fm.errorcheck(env)) {
		print("Incorrect environment specified, call env<-fm.Init(n) first.");
		return (NULL);
	}	
	if(env$m!=length(v)) {
		print("The environment mismatches the dimension to the fuzzy measure.");
		return (NULL);
	}


    EntropyChoquetVal <- -1;  # this is just a initial value.
    EntropyChoquetValue <- .C("EntropyChoquetCall", 
        as.numeric(v),
        as.integer(log2(length(v))),
        out = as.numeric(EntropyChoquetVal), 
	 as.integer(env$m), as.integer(env$card), as.integer(env$cardpos),as.double(env$bit2card),as.double(env$card2bit),as.double(env$factorials)
);
    return (EntropyChoquetValue$out);
}


fm.EntropyChoquetMob <- function(Mob,env=NULL)
{
    # Calculates entropy value of the Choquet integral of fuzzy measure in general representation.
	if(fm.errorcheck(env)) {
		print("Incorrect environment specified, call env<-fm.Init(n) first.");
		return (NULL);
	} 

	if(env$m!=length(Mob)) {
		print("The environment mismatches the dimension to the fuzzy measure.");
		return (NULL);
	}

    v = fm.Zeta(Mob,env);
	
    EntropyChoquetVal <- -1;  # this is just a initial value.
    EntropyChoquetValue <- .C("EntropyChoquetCall", 
        as.numeric(v),
        as.integer(log2(length(v))),
        out = as.numeric(EntropyChoquetVal), 
	 as.integer(env$m), as.integer(env$card), as.integer(env$cardpos),as.double(env$bit2card),as.double(env$card2bit),as.double(env$factorials)
);
    return (EntropyChoquetValue$out);
}


fm.fitting<- function(data, env=NULL, kadd="NA")
{
	# This function estimates the values of a k-additive standard fuzzy measure based on empirical data. 
	# The result is an array containing the values of the fuzzy measure in Mobius, ordered according to set cardinalities.
	# kadd define the complexity of fuzzy measure. if kadd is not provided, its default value is equal to the number of inputs.

	size <- dim(as.matrix(data));
	n <- size[2] - 1;
	datanum <- size[1];
	m <- 2^n;
	MobiusVal <- array(0.0,m);


	if (kadd == "NA") 
	{
		kadd = n;
    }
  	
    MobiusValue <- .C("fittingCall", as.integer(n),
        as.integer(datanum),
        as.integer(kadd),
        out = as.numeric(MobiusVal),
        as.numeric(t(data))
    );
				
	return (MobiusValue$out);
}


fm.fittingMob<- function(data, env=NULL, kadd="NA")
{
	# This function estimates the values of a k-additive Mobius fuzzy measure based on empirical data. 
	# The result is an array containing the values of the fuzzy measure in Mobius, ordered according to set cardinalities.
	# kadd define the complexity of fuzzy measure. if kadd is not provided, its default value is equal to the number of inputs.

	size <- dim(as.matrix(data));
	n <- size[2] - 1;
	datanum <- size[1];
	m <- 2^n;
	MobiusVal <- array(0.0,m);
	
	if (kadd == "NA") 
	{
		kadd = n;
    }
  	
    MobiusValue <- .C("fittingCallMob", as.integer(n),
        as.integer(datanum),
        as.integer(kadd),
        out = as.numeric(MobiusVal),
        as.numeric(t(data))
    );
					
	return (MobiusValue$out);
}


fm.FuzzyMeasureFitLP <- function(data, env=NULL, kadd="NA", 
        options=0, indexlow=(NULL), indexhigh=(NULL) , option1=0, orness=(NULL))
{
	# This function estimates the values of a k-additive fuzzy measure based on empirical data. 
	# The result is an array containing the values of a standard fuzzy measure, ordered according to set cardinalities.
	# kadd define the complexity of fuzzy measure. if kadd is not provided, its default value is equal to the number of inputs.
    # int FuzzyMeasureFitLP(int n, int m, int K, int Kadd, double *v, double* XYData, int options=0, 
    #    double* indexlow=(NULL), double* indexhigh=(NULL) , int option1=0, double* orness=(NULL));
    # Input parameters: 
    # n - the dimension of inputs, m = 2^n - the number of fuzzy measure values
    # K - the number of empirical data
    # Kadd - k in k-additive f. measures, 1 < Kadd < n+1. Kdd=n - f.m. is unrestricted
    # XYData - an array of size K x (n+1), where each row is the pair (x,y), K data altogether
    # options (default value is 0)
    #    1 - lower bounds on Shapley values supplied in indexlow
    #    2 - upper bounds on Shapley values supplied in indexhigh
    #    3 - lower and upper bounds on Shapley values supplied in indexlow and indexhigh
    #    4 - lower bounds on all interaction indices supplied in indexlow
    #    5 - upper bounds on all interaction indices supplied in indexhigh
    #    6 - lower and upper bounds on all interaction indices supplied inindexlow and indexhigh
    #    all these value will be treated as additional constraints in the LP
    # indexlow, indexhigh - array of size n (options =1,2,3) or m (options=4,5,6)
    # containing the lower and upper bounds on the Shapley values or interaction indices
    # Example of orness in C:
	# double orness[2];
	# orness[0]=0; 
	# orness[1]=1;

	size <- dim(as.matrix(data));
	n <- size[2] - 1;
	datanum <- size[1];
	m <- 2^n;
	MobiusVal <- array(0.0,m);
	
	if (kadd == "NA") 
	{
		kadd = n;
    }
  	opt=options+128; # means conversion to standard at the end
    MobiusValue <- .C("FuzzyMeasureFitLPCall", as.integer(n),
        as.integer(datanum),
        as.integer(kadd),
        out = as.numeric(MobiusVal),
        as.numeric(t(data)),
        as.integer(opt), 
        as.numeric(indexlow), 
        as.numeric(indexhigh), 
        as.integer(option1), 
        as.numeric(orness)
    );
#	print(	MobiusValue );
			
	return (MobiusValue$out);
}

fm.fittingKtolerant<- function(data, env=NULL, kadd="NA")
{
	# This function estimates the values of a k-additive standard fuzzy measure based on empirical data. 
	# The result is an array containing the values of the fuzzy measure in Mobius, ordered according to set cardinalities.
	# kadd define the complexity of fuzzy measure. if kadd is not provided, its default value is equal to the number of inputs.

	size <- dim(as.matrix(data));
	n <- size[2] - 1;
	datanum <- size[1];
	m <- 2^n;
	Val <- array(0.0,m);


	if (kadd == "NA") 
	{
		kadd = n;
    }
  	
    Value <- .C("fittingCallKtolerant", as.integer(n),
        as.integer(datanum),
        as.integer(kadd),
        out = as.numeric(Val),
        as.numeric(t(data))
    );
				
	return (Value$out);
}
fm.fittingKmaxitive<- function(data, env=NULL, kadd="NA")
{
	# This function estimates the values of a k-additive standard fuzzy measure based on empirical data. 
	# The result is an array containing the values of the fuzzy measure in Mobius, ordered according to set cardinalities.
	# kadd define the complexity of fuzzy measure. if kadd is not provided, its default value is equal to the number of inputs.

	size <- dim(as.matrix(data));
	n <- size[2] - 1;
	datanum <- size[1];
	m <- 2^n;
	Val <- array(0.0,m);


	if (kadd == "NA") 
	{
		kadd = n;
    }
  	
    Value <- .C("fittingCallKmaxitive", as.integer(n),
        as.integer(datanum),
        as.integer(kadd),
        out = as.numeric(Val),
        as.numeric(t(data))
    );
			
	return (Value$out);
}



fm.fittingKinteractive<- function(data, env=NULL, kadd="NA", K="NA")
{
	# This function estimates the values of a k-interactive standard fuzzy measure based on empirical data. 
	# The result is an array containing the values of the fuzzy measure in Mobius, ordered according to set cardinalities.
	# kadd define the complexity of fuzzy measure. if kadd is not provided, its default value is 2.

	size <- dim(as.matrix(data));
	n <- size[2] - 1;
	datanum <- size[1];
	m <- 2^n;
	Val <- array(0.0,m);


	if (kadd == "NA") 
	{
		kadd = 2;
    }
	if(K == "NA")
	{
		K=0.5;
	}
  	
    Value <- .C("fittingCallKinteractive", as.integer(n),
        as.integer(datanum),
        as.integer(kadd),
        out = as.numeric(Val),
        as.numeric(t(data)),
		as.numeric(K)
    );
			
	return (Value$out);
}

fm.fittingKinteractiveAuto<- function(data, env=NULL, kadd="NA")
{
	# This function estimates the values of a k-interactive standard fuzzy measure based on empirical data. 
	# The result is an array containing the values of the fuzzy measure in Mobius, ordered according to set cardinalities.
	# kadd define the complexity of fuzzy measure. if kadd is not provided, its default value is 2.

	size <- dim(as.matrix(data));
	n <- size[2] - 1;
	datanum <- size[1];
	m <- 2^n;
	Val <- array(0.0,m);


	if (kadd == "NA") 
	{
		kadd = 2;
    }
	K=0.5;
  	
    Value <- .C("fittingCallKinteractiveAuto", as.integer(n),
        as.integer(datanum),
        as.integer(kadd),
        out = as.numeric(Val),
        as.numeric(t(data)),
		as.numeric(K),
		200
    );
		
	return (Value$out);
}

fm.fittingKinteractiveMC<- function(data, env=NULL, kadd="NA", K="NA")
{
	# This function estimates the values of a k-interactive standard fuzzy measure based on empirical data. 
	# The result is an array containing the values of the fuzzy measure in Mobius, ordered according to set cardinalities.
	# kadd define the complexity of fuzzy measure. if kadd is not provided, its default value is 2.

	size <- dim(as.matrix(data));
	n <- size[2] - 1;
	datanum <- size[1];
	m <- 2^n;
	Val <- array(0.0,m);


	if (kadd == "NA") 
	{
		kadd = 2;
    }
	if(K == "NA")
	{
		K=0.5;
	}
  	
    Value <- .C("fittingCallKinteractiveMC", as.integer(n),
        as.integer(datanum),
        as.integer(kadd),
        out = as.numeric(Val),
        as.numeric(t(data)),
		as.numeric(K)
    );
			
	return (Value$out);
}


fm.fittingKinteractiveMarginal<- function(data, env=NULL, kadd="NA", K="NA", submod="NA")
{
	# This function estimates the values of a k-interactive standard fuzzy measure based on empirical data. 
	# The result is an array containing the values of the fuzzy measure in Mobius, ordered according to set cardinalities.
	# kadd define the complexity of fuzzy measure. if kadd is not provided, its default value is 2.

	size <- dim(as.matrix(data));
	n <- size[2] - 1;
	datanum <- size[1];
	m <- 2^n;
	Val <- array(0.0,m);


	if (kadd == "NA") 
	{
		kadd = 2;
    }
	if(K == "NA")
	{
		K=0.5;
	}
  	submodular=submod;
	if(submodular == "NA") {submodular=0;}

    Value <- .C("fittingCallKinteractiveMarginal", as.integer(n),
        as.integer(datanum),
        as.integer(kadd),
        out = as.numeric(Val),
        as.numeric(t(data)),
		as.numeric(K), 
		as.integer(submodular)
    );
			
	return (Value$out);
}

fm.fittingKinteractiveMarginalMC<- function(data, env=NULL, kadd="NA", K="NA", submod="NA")
{
	# This function estimates the values of a k-interactive standard fuzzy measure based on empirical data. 
	# The result is an array containing the values of the fuzzy measure in Mobius, ordered according to set cardinalities.
	# kadd define the complexity of fuzzy measure. if kadd is not provided, its default value is 2.

	size <- dim(as.matrix(data));
	n <- size[2] - 1;
	datanum <- size[1];
	m <- 2^n;
	Val <- array(0.0,m);


	if (kadd == "NA") 
	{
		kadd = 2;
    }
	if(K == "NA")
	{
		K=0.5;
	}
  	
    Value <- .C("fittingCallKinteractiveMarginalMC", as.integer(n),
        as.integer(datanum),
        as.integer(kadd),
        out = as.numeric(Val),
        as.numeric(t(data)),
		as.numeric(K)
    );
			
	return (Value$out);
}




fm.FuzzyMeasureFitLPMob <- function(data, env=NULL, kadd="NA", 
        options=0, indexlow=(NULL), indexhigh=(NULL) , option1=0, orness=(NULL))
{
	# This function estimates the values of a k-additive fuzzy measure based on empirical data. 
	# The result is an array containing the values of the fuzzy measure in Mobius, ordered according to set cardinalities.
	# kadd define the complexity of fuzzy measure. if kadd is not provided, its default value is equal to the number of inputs.
    # int FuzzyMeasureFitLP(int n, int m, int K, int Kadd, double *v, double* XYData, int options=0, 
    #    double* indexlow=(NULL), double* indexhigh=(NULL) , int option1=0, double* orness=(NULL));
    # Input parameters: 
    # n - the dimension of inputs, m = 2^n - the number of fuzzy measure values
    # K - the number of empirical data
    # Kadd - k in k-additive f. measures, 1 < Kadd < n+1. Kdd=n - f.m. is unrestricted
    # XYData - an array of size K x (n+1), where each row is the pair (x,y), K data altogether
    # options (default value is 0)
    #    1 - lower bounds on Shapley values supplied in indexlow
    #    2 - upper bounds on Shapley values supplied in indexhigh
    #    3 - lower and upper bounds on Shapley values supplied in indexlow and indexhigh
    #    4 - lower bounds on all interaction indices supplied in indexlow
    #    5 - upper bounds on all interaction indices supplied in indexhigh
    #    6 - lower and upper bounds on all interaction indices supplied inindexlow and indexhigh
    #    all these value will be treated as additional constraints in the LP
    # indexlow, indexhigh - array of size n (options =1,2,3) or m (options=4,5,6)
    # containing the lower and upper bounds on the Shapley values or interaction indices
    # Example of orness in C:
	# double orness[2];
	# orness[0]=0; 
	# orness[1]=1;

	size <- dim(as.matrix(data));
	n <- size[2] - 1;
	datanum <- size[1];
	m <- 2^n;
	MobiusVal <- array(0.0,m);
	
	if (kadd == "NA") 
	{
		kadd = n;
    }
  	
    MobiusValue <- .C("FuzzyMeasureFitLPCall", as.integer(n),
        as.integer(datanum),
        as.integer(kadd),
        out = as.numeric(MobiusVal),
        as.numeric(t(data)),
        as.integer(options), 
        as.numeric(indexlow), 
        as.numeric(indexhigh), 
        as.integer(option1), 
        as.numeric(orness)
    );
					
	return (MobiusValue$out);
}


fm.Interaction <- function(v,env=NULL)
{
	# calculates all interaction indices 
	# result is a matrix, whose first column is the interaction index
	# and second column is the index of #ition.
	if(fm.errorcheck(env)) {
		print("Incorrect environment specified, call env<-fm.Init(n) first.");
		return (NULL);
	}

	if(env$m!=length(v)) {
		print("The environment mismatches the dimension to the fuzzy measure.");
		return (NULL);
	}

    coalition <- array(0,length(v));
    InteractionVal <- array(0.0,length(v));
    InteractionValue <- .C("InteractionCall", as.numeric(v), 
 		                   inter = as.numeric(InteractionVal),
						   #as.integer(log2(length(Mob))),
					   #coal = as.integer(coalition), 
	 as.integer(env$m), as.integer(env$card), as.integer(env$cardpos),as.double(env$bit2card),as.double(env$card2bit),as.double(env$factorials)
);
	Co <- .C("ShowCoalitionsCall", as.integer(env$m), coal = as.integer(coalition)); 
    coalIndex <- as.matrix(Co$coal);
    inteIndex <- as.matrix(InteractionValue$inter);
    index <- cbind(inteIndex,coalIndex); 				
    return (round(index, digits=4));
}


fm.InteractionMob <- function(Mob,env=NULL)
{
	# calculates all interaction indices 
	# result is a matrix, whose first column is the interaction index
	# and second column is the index of coalition.
	if(fm.errorcheck(env)) {
		print("Incorrect environment specified, call env<-fm.Init(n) first.");
		return (NULL);
	}
	if(env$m!=length(Mob)) {
		print("The environment mismatches the dimension to the fuzzy measure.");
		return (NULL);
	}

	 coalition <- array(0,length(Mob));
	 InteractionVal <- array(0.0,length(Mob));
	 InteractionValue <- .C("InteractionMobCall", as.numeric(Mob), 
			                   inter = as.numeric(InteractionVal),
						   #as.integer(log2(length(Mob))),
					  # coal = as.integer(coalition), 
	 as.integer(env$m), as.integer(env$card), as.integer(env$cardpos),as.double(env$bit2card),as.double(env$card2bit),as.double(env$factorials)
);
	inteIndex <- as.matrix(InteractionValue$inter);

	Co <- .C("ShowCoalitionsCall", as.integer(env$m), coal = as.integer(coalition)); 
      coalIndex <- as.matrix(Co$coal);
	index <- cbind(inteIndex,coalIndex); 				
	return (round(index, digits=4));
}


fm.InteractionB <- function(v,env=NULL)
{
	# calculates all InteractionB indices 
	# result is a matrix, whose first column is the InteractionB index
	# and second column is the index of coalition.
	if(fm.errorcheck(env)) {
		print("Incorrect environment specified, call env<-fm.Init(n) first.");
		return (NULL);
	}
	if(env$m!=length(v)) {
		print("The environment mismatches the dimension to the fuzzy measure.");
		return (NULL);
	}


    coalition <- array(0,length(v));
    InteractionBVal <- array(0.0,length(v));
    InteractionBValue <- .C("InteractionBCall", as.numeric(v), 
		                   inter = as.numeric(InteractionBVal),
						   #as.integer(log2(length(Mob))),
                          # coal = as.integer(coalition), 
	 as.integer(env$m), as.integer(env$card), as.integer(env$cardpos),as.double(env$bit2card),as.double(env$card2bit),as.double(env$factorials)
);
	inteIndex <- as.matrix(InteractionBValue$inter);

	Co <- .C("ShowCoalitionsCall", as.integer(env$m), coal = as.integer(coalition)); 
      coalIndex <- as.matrix(Co$coal);
	index <- cbind(inteIndex,coalIndex); 				
	return (round(index, digits=4));
}


fm.InteractionBMob <- function(Mob,env=NULL)
{
	# calculates all InteractionB indices 
	# result is a matrix, whose first column is the InteractionB index
	# and second column is the index of coalition.
	if(fm.errorcheck(env)) {
		print("Incorrect environment specified, call env<-fm.Init(n) first.");
		return (NULL);
	}
	if(env$m!=length(Mob)) {
		print("The environment mismatches the dimension to the fuzzy measure.");
		return (NULL);
	}

    coalition <- array(0,length(Mob));
    InteractionBVal <- array(0.0,length(Mob));
    InteractionBValue <- .C("InteractionBMobCall", as.numeric(Mob), 
		                   inter = as.numeric(InteractionBVal),
						   #as.integer(log2(length(Mob))),
	                       #coal = as.integer(coalition), 
	 as.integer(env$m), as.integer(env$card), as.integer(env$cardpos),as.double(env$bit2card),as.double(env$card2bit),as.double(env$factorials)
);
	inteIndex <- as.matrix(InteractionBValue$inter);

	Co <- .C("ShowCoalitionsCall", as.integer(env$m), coal = as.integer(coalition)); 
      coalIndex <- as.matrix(Co$coal);
	index <- cbind(inteIndex,coalIndex); 				
	return (round(index, digits=4));
}


fm.Bipartition <- function(v,env=NULL)
{
	# calculates all interaction indices 
	# result is a matrix, whose first column is the interaction index
	# and second column is the index of coalition.
	if(fm.errorcheck(env)) {
		print("Incorrect environment specified, call env<-fm.Init(n) first.");
		return (NULL);
	}

	if(env$m!=length(v)) {
		print("The environment mismatches the dimension to the fuzzy measure.");
		return (NULL);
	}


    coalition <- array(0,length(v));
    InteractionVal <- array(0.0,length(v));
    InteractionValue <- .C("BipartitionShapleyCall", as.numeric(v), 
 		                   inter = as.numeric(InteractionVal),
						   as.integer(log2(length(v))),
					   #coal = as.integer(coalition), 
	 as.integer(env$m), as.integer(env$card), as.integer(env$cardpos),as.double(env$bit2card),as.double(env$card2bit),as.double(env$factorials));
    inteIndex <- as.matrix(InteractionValue$inter);

    Co <- .C("ShowCoalitionsCall", as.integer(env$m), coal = as.integer(coalition)); 
    coalIndex <- as.matrix(Co$coal);
    index <- cbind(inteIndex,coalIndex); 				
    return (round(index, digits=4));
}

fm.BipartitionBanzhaf <- function(v,env=NULL)
{
	# calculates all interaction indices 
	# result is a matrix, whose first column is the interaction index
	# and second column is the index of coalition.
	if(fm.errorcheck(env)) {
		print("Incorrect environment specified, call env<-fm.Init(n) first.");
		return (NULL);
	}

	if(env$m!=length(v)) {
		print("The environment mismatches the dimension to the fuzzy measure.");
		return (NULL);
	}


    coalition <- array(0,length(v));
    InteractionVal <- array(0.0,length(v));
    InteractionValue <- .C("BipartitionBanzhafCall", as.numeric(v), 
 		                   inter = as.numeric(InteractionVal),
						   as.integer(log2(length(v))),
					   #coal = as.integer(coalition), 
	 as.integer(env$m), as.integer(env$card), as.integer(env$cardpos),as.double(env$bit2card),as.double(env$card2bit),as.double(env$factorials));
    inteIndex <- as.matrix(InteractionValue$inter);

	Co <- .C("ShowCoalitionsCall", as.integer(env$m), coal = as.integer(coalition)); 
      coalIndex <- as.matrix(Co$coal);
    index <- cbind(inteIndex,coalIndex); 				
    return (round(index, digits=4));
}

fm.NonadditivityIndex <- function(v,env=NULL)
{
	# calculates all interaction indices 
	# result is a matrix, whose first column is the interaction index
	# and second column is the index of coalition.
	if(fm.errorcheck(env)) {
		print("Incorrect environment specified, call env<-fm.Init(n) first.");
		return (NULL);
	}

	if(env$m!=length(v)) {
		print("The environment mismatches the dimension to the fuzzy measure.");
		return (NULL);
	}

    coalition <- array(0,length(v));
    InteractionVal <- array(0.0,length(v));
    InteractionValue <- .C("NonadditivityIndexCall", as.numeric(v), 
 		                   inter = as.numeric(InteractionVal),
						   as.integer(log2(length(v))),
					   #coal = as.integer(coalition), 
	 as.integer(env$m), as.integer(env$card), as.integer(env$cardpos),as.double(env$bit2card),as.double(env$card2bit),as.double(env$factorials));
    inteIndex <- as.matrix(InteractionValue$inter);

 	Co <- .C("ShowCoalitionsCall", as.integer(env$m), coal = as.integer(coalition)); 
      coalIndex <- as.matrix(Co$coal);
    index <- cbind(inteIndex,coalIndex); 				
    return (round(index, digits=4));
}



fm.NonadditivityIndexMob <- function(Mob,env=NULL)
{
	# calculates all interaction indices 
	# result is a matrix, whose first column is the interaction index
	# and second column is the index of coalition.
	if(fm.errorcheck(env)) {
		print("Incorrect environment specified, call env<-fm.Init(n) first.");
		return (NULL);
	}

	if(env$m!=length(Mob)) {
		print("The environment mismatches the dimension to the fuzzy measure.");
		return (NULL);
	}


    coalition <- array(0,length(Mob));
    InteractionVal <- array(0.0,length(Mob));
    InteractionValue <- .C("NonadditivityIndexMobCall", as.numeric(Mob), 
 		                   inter = as.numeric(InteractionVal),
						   as.integer(log2(length(Mob))),
					   #coal = as.integer(coalition), 
	 as.integer(env$m), as.integer(env$card), as.integer(env$cardpos),as.double(env$bit2card),as.double(env$card2bit),as.double(env$factorials)
);
    inteIndex <- as.matrix(InteractionValue$inter);

	Co <- .C("ShowCoalitionsCall", as.integer(env$m), coal = as.integer(coalition)); 
      coalIndex <- as.matrix(Co$coal);
    index <- cbind(inteIndex,coalIndex); 				
    return (round(index, digits=4));
}

fm.IsMeasureAdditive <- function(v,env=NULL)
{
	# Returns 1 if yes, 0 if no;
	if(fm.errorcheck(env)) {
		print("Incorrect environment specified, call env<-fm.Init(n) first.");
		return (NULL);
	}
	if(env$m!=length(v)) {
		print("The environment mismatches the dimension to the fuzzy measure.");
		return (NULL);
	}

    # v is a fuzzy measure in standard representation.
	result <- 1;
    res <- .C("IsMeasureAdditiveCall", 
        as.numeric(v), 
        result=as.integer(result), 
	 as.integer(env$m), as.integer(env$card), as.integer(env$cardpos),as.double(env$bit2card),as.double(env$card2bit),as.double(env$factorials)

    );
	return (as.logical(res$result));
}


fm.IsMeasureAdditiveMob <- function(Mob,env=NULL)
{
	# Returns 1 if yes, 0 if no;
	if(fm.errorcheck(env)) {
		print("Incorrect environment specified, call env<-fm.Init(n) first.");
		return (NULL);
	}

	if(env$m!=length(Mob)) {
		print("The environment mismatches the dimension to the fuzzy measure.");
		return (NULL);
	}

	result <- 1;
    # v is a fuzzy measure in standard representation.
    res <- .C("IsMeasureAdditiveMobCall", 
        as.numeric(Mob), 
        result=as.integer(result), 
	 as.integer(env$m), as.integer(env$card), as.integer(env$cardpos),as.double(env$bit2card),as.double(env$card2bit),as.double(env$factorials)

    );
	return (as.logical(res$result));
}


fm.IsMeasureBalanced <- function(v,env=NULL)
{
	# Returns 1 if yes, 0 if no;
	if(fm.errorcheck(env)) {
		print("Incorrect environment specified, call env<-fm.Init(n) first.");
		return (NULL);
	}
	if(env$m!=length(v)) {
		print("The environment mismatches the dimension to the fuzzy measure.");
		return (NULL);
	}

    # v is a fuzzy measure in standard representation.
	result <- 1;
    res <- .C("IsMeasureBalancedCall", 
        as.numeric(v), 
        result=as.integer(result), 
	 as.integer(env$m), as.integer(env$card), as.integer(env$cardpos),as.double(env$bit2card),as.double(env$card2bit),as.double(env$factorials)

    );
	return (as.logical(res$result));
}


fm.IsMeasureBalancedMob <- function(Mob,env=NULL)
{
	# Returns 1 if yes, 0 if no;
	if(fm.errorcheck(env)) {
		print("Incorrect environment specified, call env<-fm.Init(n) first.");
		return (NULL);
	}
	if(env$m!=length(Mob)) {
		print("The environment mismatches the dimension to the fuzzy measure.");
		return (NULL);
	}
;

	result <- 1;
    res <- .C("IsMeasureBalancedMobCall", 
        as.numeric(Mob), 
         result=as.integer(result), 
	 as.integer(env$m), as.integer(env$card), as.integer(env$cardpos),as.double(env$bit2card),as.double(env$card2bit),as.double(env$factorials)

    );
	return (as.logical(res$result));
}


fm.IsMeasureSelfdual <- function(v,env=NULL)
{
	# Returns 1 if yes, 0 if no;
	if(fm.errorcheck(env)) {
		print("Incorrect environment specified, call env<-fm.Init(n) first.");
		return (NULL);
	}
	if(env$m!=length(v)) {
		print("The environment mismatches the dimension to the fuzzy measure.");
		return (NULL);
	}
    # v is a fuzzy measure in standard representation.
	result <- 1;
    res <- .C("IsMeasureSelfdualCall", 
        as.numeric(v), 
         result=as.integer(result), 
	 as.integer(env$m), as.integer(env$card), as.integer(env$cardpos),as.double(env$bit2card),as.double(env$card2bit),as.double(env$factorials)

    );
	return (as.logical(res$result));
}


fm.IsMeasureSelfdualMob <- function(Mob,env=NULL)
{
	# Returns 1 if yes, 0 if no;
	if(fm.errorcheck(env)) {
		print("Incorrect environment specified, call env<-fm.Init(n) first.");
		return (NULL);
	}
	if(env$m!=length(Mob)) {
		print("The environment mismatches the dimension to the fuzzy measure.");
		return (NULL);
	}


	result <- 1;
    res <- .C("IsMeasureSelfdualMobCall", 
        as.numeric(Mob), 
        result=as.integer(result), 
	 as.integer(env$m), as.integer(env$card), as.integer(env$cardpos),as.double(env$bit2card),as.double(env$card2bit),as.double(env$factorials)

    );
	return (as.logical(res$result));
}


fm.IsMeasureSubadditive <- function(v,env=NULL)
{
	# Returns 1 if yes, 0 if no;
	if(fm.errorcheck(env)) {
		print("Incorrect environment specified, call env<-fm.Init(n) first.");
		return (NULL);
	}
	if(env$m!=length(v)) {
		print("The environment mismatches the dimension to the fuzzy measure.");
		return (NULL);
	}
    # v is a fuzzy measure in standard representation.
	result <- 1;
    res <- .C("IsMeasureSubadditiveCall", 
        as.numeric(v), 
         result=as.integer(result), 
	 as.integer(env$m), as.integer(env$card), as.integer(env$cardpos),as.double(env$bit2card),as.double(env$card2bit),as.double(env$factorials)

    );
	return (as.logical(res$result));
}


fm.IsMeasureSubadditiveMob <- function(Mob,env=NULL)
{
	# Returns 1 if yes, 0 if no;
	if(fm.errorcheck(env)) {
		print("Incorrect environment specified, call env<-fm.Init(n) first.");
		return (NULL);
	}
	if(env$m!=length(Mob)) {
		print("The environment mismatches the dimension to the fuzzy measure.");
		return (NULL);
	}

	result <- 1;
    res <- .C("IsMeasureSubadditiveMobCall", 
        as.numeric(Mob), 
         result=as.integer(result), 
	 as.integer(env$m), as.integer(env$card), as.integer(env$cardpos),as.double(env$bit2card),as.double(env$card2bit),as.double(env$factorials)

    );
	return (as.logical(res$result));
}


fm.IsMeasureSubmodular <- function(v,env=NULL)
{
	if(fm.errorcheck(env)) {
		print("Incorrect environment specified, call env<-fm.Init(n) first.");
		return (NULL);
	}
	if(env$m!=length(v)) {
		print("The environment mismatches the dimension to the fuzzy measure.");
		return (NULL);
	}
	# Returns 1 if yes, 0 if no;
    # v is a fuzzy measure in standard representation.
	result <- 1;
    res <- .C("IsMeasureSubmodularCall", 
        as.numeric(v), 
        result=as.integer(result), 
	 as.integer(env$m), as.integer(env$card), as.integer(env$cardpos),as.double(env$bit2card),as.double(env$card2bit),as.double(env$factorials)

    );
	return (as.logical(res$result));
}


fm.IsMeasureSubmodularMob <- function(Mob,env=NULL)
{
	if(fm.errorcheck(env)) {
		print("Incorrect environment specified, call env<-fm.Init(n) first.");
		return (NULL);
	}
	if(env$m!=length(Mob)) {
		print("The environment mismatches the dimension to the fuzzy measure.");
		return (NULL);
	}
	# Returns 1 if yes, 0 if no;


	result <- 1;
    res <- .C("IsMeasureSubmodularMobCall", 
        as.numeric(Mob), 
         result=as.integer(result), 
	 as.integer(env$m), as.integer(env$card), as.integer(env$cardpos),as.double(env$bit2card),as.double(env$card2bit),as.double(env$factorials)

    );
	return (as.logical(res$result));
}


fm.IsMeasureSuperadditive <- function(v,env=NULL)
{
	# Returns 1 if yes, 0 if no;
	if(fm.errorcheck(env)) {
		print("Incorrect environment specified, call env<-fm.Init(n) first.");
		return (NULL);
	}
	if(env$m!=length(v)) {
		print("The environment mismatches the dimension to the fuzzy measure.");
		return (NULL);
	}

    # v is a fuzzy measure in standard representation.
	result <- 1;
    res <- .C("IsMeasureSuperadditiveCall", 
        as.numeric(v), 
        result=as.integer(result), 
	 as.integer(env$m), as.integer(env$card), as.integer(env$cardpos),as.double(env$bit2card),as.double(env$card2bit),as.double(env$factorials)

    );
	return (as.logical(res$result));
}


fm.IsMeasureSuperadditiveMob <- function(Mob,env=NULL)
{
	# Returns 1 if yes, 0 if no;
	if(fm.errorcheck(env)) {
		print("Incorrect environment specified, call env<-fm.Init(n) first.");
		return (NULL);
	}
	if(env$m!=length(Mob)) {
		print("The environment mismatches the dimension to the fuzzy measure.");
		return (NULL);
	}


	result <- 1;
    res <- .C("IsMeasureSuperadditiveMobCall", 
        as.numeric(Mob), 
         result=as.integer(result), 
	 as.integer(env$m), as.integer(env$card), as.integer(env$cardpos),as.double(env$bit2card),as.double(env$card2bit),as.double(env$factorials)

    );
	return (as.logical(res$result));
}


fm.IsMeasureSupermodular <- function(v,env=NULL)
{
	# Returns 1 if yes, 0 if no;
	if(fm.errorcheck(env)) {
		print("Incorrect environment specified, call env<-fm.Init(n) first.");
		return (NULL);
	}
	if(env$m!=length(v)) {
		print("The environment mismatches the dimension to the fuzzy measure.");
		return (NULL);
	}

    # v is a fuzzy measure in standard representation.
	result <- 1;
    res <- .C("IsMeasureSupermodularCall", 
        as.numeric(v), 
         result=as.integer(result), 
	 as.integer(env$m), as.integer(env$card), as.integer(env$cardpos),as.double(env$bit2card),as.double(env$card2bit),as.double(env$factorials)

    );
	return (as.logical(res$result));
}


fm.IsMeasureSupermodularMob <- function(Mob,env=NULL)
{
	# Returns 1 if yes, 0 if no;
	if(fm.errorcheck(env)) {
		print("Incorrect environment specified, call env<-fm.Init(n) first.");
		return (NULL);
	}
	if(env$m!=length(Mob)) {
		print("The environment mismatches the dimension to the fuzzy measure.");
		return (NULL);
	}
    # Mob is a fuzzy measure in Mobius representation.
	result <- 1;

    res <- .C("IsMeasureSupermodularMobCall", 
        as.numeric(Mob), 
        result=as.integer(result), 
	 as.integer(env$m), as.integer(env$card), as.integer(env$cardpos),as.double(env$bit2card),as.double(env$card2bit),as.double(env$factorials)

    );
	return (as.logical(res$result));
}


fm.IsMeasureSymmetric <- function(v, env=NULL)
{
	# Returns 1 if yes, 0 if no;
	if(fm.errorcheck(env)) {
		print("Incorrect environment specified, call env<-fm.Init(n) first.");
		return (NULL);
	}
	if(env$m!=length(v)) {
		print("The environment mismatches the dimension to the fuzzy measure.");
		return (NULL);
	}

    # v is a fuzzy measure in standard representation.
	result <- 1;
    res <- .C("IsMeasureSymmetricCall", 
        as.numeric(v), 
       result=as.integer(result), 
	 as.integer(env$m), as.integer(env$card), as.integer(env$cardpos),as.double(env$bit2card),as.double(env$card2bit),as.double(env$factorials)

       );

	return (as.logical(res$result));
}


fm.IsMeasureSymmetricMob <- function(Mob,env=NULL)
{
	# Returns 1 if yes, 0 if no;
	if(fm.errorcheck(env)) {
		print("Incorrect environment specified, call env<-fm.Init(n) first.");
		return (NULL);
	}
	if(env$m!=length(Mob)) {
		print("The environment mismatches the dimension to the fuzzy measure.");
		return (NULL);
	}
    # Mob is a fuzzy measure in Mobius representation.

	result <- 1;
    res <- .C("IsMeasureSymmetricMobCall", 
        as.numeric(Mob), 
         result=as.integer(result), 
	 as.integer(env$m), as.integer(env$card), as.integer(env$cardpos),as.double(env$bit2card),as.double(env$card2bit),as.double(env$factorials)

    );
	return (as.logical(res$result));
}

fm.IsMeasureKmaxitive <- function(v, env=NULL)
{
	# Returns 1 if yes, 0 if no;
	if(fm.errorcheck(env)) {
		print("Incorrect environment specified, call env<-fm.Init(n) first.");
		return (NULL);
	}
	if(env$m!=length(v)) {
		print("The environment mismatches the dimension to the fuzzy measure.");
		return (NULL);
	}

    # v is a fuzzy measure in standard representation.
	result <- 1;
    res <- .C("IsMeasureKmaxitiveCall", 
        as.numeric(v), 
        result=as.integer(result), 
	 as.integer(env$m), as.integer(env$card), as.integer(env$cardpos),as.double(env$bit2card),as.double(env$card2bit),as.double(env$factorials)

       );

	return (as.integer(res$result));
}

fm.IsMeasureKmaxitiveMob <- function(Mob,env=NULL)
{
	# Returns 1 if yes, 0 if no;
	if(fm.errorcheck(env)) {
		print("Incorrect environment specified, call env<-fm.Init(n) first.");
		return (NULL);
	}
	if(env$m!=length(Mob)) {
		print("The environment mismatches the dimension to the fuzzy measure.");
		return (NULL);
	}
    # Mob is a fuzzy measure in Mobius representation.

	result <- 1;
    res <- .C("IsMeasureKmaxitiveMobCall", 
        as.numeric(Mob), 
        result=as.integer(result), 
	 as.integer(env$m), as.integer(env$card), as.integer(env$cardpos),as.double(env$bit2card),as.double(env$card2bit),as.double(env$factorials)

    );
	return (as.integer(res$result));
}



fm.Mobius <- function(v,env=NULL)
{
    # Calculates Mobius representation of the general fuzzy measure v
	if(fm.errorcheck(env)) {
		print("Incorrect environment specified, call env<-fm.Init(n) first.");
		return (NULL);
	}
	if(env$m!=length(v)) {
		print("The environment mismatches the dimension to the fuzzy measure.");
		return (NULL);
	}
    MobiusVal <-  array(0.0,length(v));
    MobiusValue <- .C("MobiusCall", as.numeric(v), 
        out = as.numeric(MobiusVal),
        as.integer(env$n), 
	 as.integer(env$m), as.integer(env$card), as.integer(env$cardpos),as.double(env$bit2card),as.double(env$card2bit),as.double(env$factorials)

    );			
    return (MobiusValue$out);
}


fm.OrnessChoquet <- function(v,env=NULL)
{
	# Calculates the orness value of the Choquet integral for a standard fuzzy measure.
	if(fm.errorcheck(env)) {
		print("Incorrect environment specified, call env<-fm.Init(n) first.");
		return (NULL);
	}
	if(env$m!=length(v)) {
		print("The environment mismatches the dimension to the fuzzy measure.");
		return (NULL);
	}
	Mob = fm.Mobius(v,env);

    OrnessChoquetMobVal <- -1;  # this is just a initial value.
	OrnessChoquetMobValue <- .C("OrnessChoquetMobCall", 
        as.numeric(Mob),
        as.integer(log2(length(Mob))),
        out = as.numeric(OrnessChoquetMobVal), 
	 as.integer(env$m), as.integer(env$card), as.integer(env$cardpos),as.double(env$bit2card),as.double(env$card2bit),as.double(env$factorials)

    );
	return (OrnessChoquetMobValue$out);
}


fm.OrnessChoquetMob <- function(Mob,env=NULL)
{
	# Calculates the orness value of the Choquet integral for the Mobius fuzzy measure.
		if(fm.errorcheck(env)) {
		print("Incorrect environment specified, call env<-fm.Init(n) first.");
		return (NULL);
	}
	if(env$m!=length(Mob)) {
		print("The environment mismatches the dimension to the fuzzy measure.");
		return (NULL);
	}
    OrnessChoquetMobVal <- -1;  # this is just a initial value.
	OrnessChoquetMobValue <- .C("OrnessChoquetMobCall", 
        as.numeric(Mob),
        as.integer(log2(length(Mob))),
        out = as.numeric(OrnessChoquetMobVal), 
	 as.integer(env$m), as.integer(env$card), as.integer(env$cardpos),as.double(env$bit2card),as.double(env$card2bit),as.double(env$factorials)

    );
	return (OrnessChoquetMobValue$out);
}


fm.Shapley<- function(v,env=NULL)
{
    # Calculates an array of Shapley values.
	if(fm.errorcheck(env)) {
		print("Incorrect environment specified, call env<-fm.Init(n) first.");
		return (NULL);
	}
	if(env$m!=length(v)) {
		print("The environment mismatches the dimension to the fuzzy measure.");
		return (NULL);
	}

    ShapleyVal <- array(0.0,log2(length(v)));
    ShapleyValue <- .C("ShapleyCall", as.numeric(v), 
        out = as.numeric(ShapleyVal),
        as.integer(log2(length(v))), 
	 as.integer(env$m), as.integer(env$card), as.integer(env$cardpos),as.double(env$bit2card),as.double(env$card2bit),as.double(env$factorials)

    );
					
    return (round(ShapleyValue$out, digits=4));
}


fm.ShapleyMob<- function(Mob,env=NULL)
{
    # Calculates an array of Shapley values.
	if(fm.errorcheck(env)) {
		print("Incorrect environment specified, call env<-fm.Init(n) first.");
		return (NULL);
	}
	if(env$m!=length(Mob)) {
		print("The environment mismatches the dimension to the fuzzy measure.");
		return (NULL);
	}


    ShapleyVal <- array(0.0,log2(length(Mob)));
    ShapleyValue <- .C("ShapleyMobCall", as.numeric(Mob), 
        out = as.numeric(ShapleyVal),
        as.integer(log2(length(Mob))), 
	 as.integer(env$m), as.integer(env$card), as.integer(env$cardpos),as.double(env$bit2card),as.double(env$card2bit),as.double(env$factorials)

    );
					
    return (round(ShapleyValue$out, digits=4));
}


fm.Sugeno <- function(x, v,env=NULL)
{
    # Calculates the value of a Sugeno integral of an input x, with provided fuzzy measure v (in general representation). 
	if(fm.errorcheck(env)) {
		print("Incorrect environment specified, call env<-fm.Init(n) first.");
		return (NULL);
	}
	if(env$m!=length(v)||env$n!=length(x)) {
		print("The environment mismatches the dimension to the fuzzy measure.");
		return (NULL);
	}
	
    SugenoVal <- -1;  # this is just a initial value.
    SugenoValue <- .C("SugenoCall", as.numeric(x),
        as.numeric(v),
        as.integer(length(x)),
        out = as.numeric(SugenoVal), 
	 as.integer(env$m), as.integer(env$card), as.integer(env$cardpos),as.double(env$bit2card),as.double(env$card2bit),as.double(env$factorials)
);
    return (SugenoValue$out);
}


fm.SugenoMob <- function(x, Mob,env=NULL)
{
    # Calculates the value of a Sugeno integral of an input x, with provided fuzzy measure v (in Mobius representation). 
	if(fm.errorcheck(env)) {
		print("Incorrect environment specified, call env<-fm.Init(n) first.");
		return (NULL);
	}
	if(env$m!=length(Mob)||env$n!=length(x)) {
		print("The environment mismatches the dimension to the fuzzy measure.");
		return (NULL);
	}

    v = fm.Zeta(Mob,env);
	
    SugenoVal <- -1;  # this is just a initial value.
    SugenoValue <- .C("SugenoCall", as.numeric(x),
        as.numeric(v),
        as.integer(length(x)),
        out = as.numeric(SugenoVal), 
	 as.integer(env$m), as.integer(env$card), as.integer(env$cardpos),as.double(env$bit2card),as.double(env$card2bit),as.double(env$factorials)
);
    return (SugenoValue$out);
}


fm.test <- function ()
{
	# Checking that the toolbox has been installed succeffully and the functions find correct values. 
	
	print("initialisation for n=3 env<-fm.Init(3)")
	env<-fm.Init(3); #print(env);

	print("Banzhaf indices fm.Banzhaf(c(0, 0.3, 0.5, 0.6, 0.4, 0.8, 0.7, 1),env)")
	print(fm.Banzhaf(c(0, 0.3, 0.5, 0.6, 0.4, 0.8, 0.7, 1),env))	

	print("Banzhaf indices for Mobius fuzzy measure fm.BanzhafMob(c(0.0, 0.3, 0.5, -0.2, 0.4, 0.1, -0.2, 0.1),env)")
	print(fm.BanzhafMob(c(0.0, 0.3, 0.5, -0.2, 0.4, 0.1, -0.2, 0.1),env))	

	print("Choquet integral for a general fuzzy measure")
	print(fm.Choquet(c(0.6, 0.3, 0.8), c(0, 0.3, 0.5, 0.6, 0.4, 0.8, 0.7, 1),env))
	
    print("Choquet integral for a Mobius fuzzy measure")
	print(fm.ChoquetMob(c(0.6, 0.3, 0.8), c(0.0, 0.3, 0.5, -0.2, 0.4, 0.1, -0.2, 0.1),env))
	
    print("ConstructLambdaMeasure in standard representation")
	print(mea<-fm.ConstructLambdaMeasure(c(0, 0.3, 0.5),env))

    print("ConstructLambdaMeasure in Mobius representation")
	print(meamob<-fm.ConstructLambdaMeasureMob(c(0, 0.3, 0.5),env))

    print("Dual measure for a fuzzy measure in standard representation")
	print(fm.dualm(mea$measure,env))

    print("Dual measure for a fuzzy measure in Mobius representation")
	print(fm.dualmMob(meamob$measure,env))

	print("Entropy value of the Choquet integral for a general fuzzy measure")
	print(fm.EntropyChoquet(c(0, 0.3, 0.5, 0.6, 0.4, 0.8, 0.7, 1),env))

    print("Entropy value of the Choquet integral for a Mobius fuzzy measure")
	print(fm.EntropyChoquetMob(c(0.0, 0.3, 0.5, -0.2, 0.4, 0.1, -0.2, 0.1),env))
	
    print("Fitting a standard fuzzy measure to data")
    d <-  matrix( c( 0.00125122, 0.563568, 0.193298, 0.164338, 
            0.808716, 0.584991, 0.479858, 0.544309, 
            0.350281, 0.895935, 0.822815, 0.625868, 
            0.746582, 0.174103, 0.858917, 0.480347, 
            0.71048, 0.513519, 0.303986, 0.387631, 
            0.0149841, 0.0914001, 0.364441, 0.134229, 
            0.147308, 0.165894, 0.988495, 0.388044, 
            0.445679, 0.11908, 0.00466919, 0.0897714, 
            0.00891113, 0.377869, 0.531647, 0.258585, 
            0.571167, 0.601746, 0.607147, 0.589803, 
            0.166229, 0.663025, 0.450775, 0.357412, 
            0.352112, 0.0570374, 0.607666, 0.270228, 
            0.783295, 0.802582, 0.519867, 0.583348, 
            0.301941, 0.875946, 0.726654, 0.562174, 
            0.955872, 0.92569, 0.539337, 0.633631, 
            0.142334, 0.462067, 0.235321, 0.228419, 
            0.862213, 0.209595, 0.779633, 0.498077, 
            0.843628, 0.996765, 0.999664, 0.930197, 
            0.611481, 0.92426, 0.266205, 0.334666, 
            0.297272, 0.840118, 0.0237427, 0.168081), 
       nrow=20, 
       ncol=4);
    print(mea1<-fm.fitting(d,env))
#    mea1<-fm.fitting(d,env)

    print("Fitting a Mobius fuzzy measure to data")
    print(fm.fittingMob(d,env))
    fm.fitting(d,env)

    print("Transform to the Mobius representation")
    print(mea1mob<-fm.Mobius(mea1,env))

	

 
	print("Interaction index for standard fuzzy measure")
	print(fm.Interaction(mea1,env))

	print("Interaction index for Mobius fuzzy measure")
	print(fm.InteractionMob(mea1mob,env))

	print("InteractionB index for a standard fuzzy measure")
	print(fm.InteractionB(mea1,env))

	print("InteractionB index for a Mobius fuzzy measure")
	print(fm.InteractionBMob(mea1mob,env))

	print("Is a standard measure additive?")
	print(fm.IsMeasureAdditive(mea1,env))

	print("Is a Mobius measure additive?")
	print(fm.IsMeasureAdditiveMob(mea1mob,env))

	print("Is a standard measure balanced?")
	print(fm.IsMeasureBalanced(mea1,env))

	print("Is a Mobius measure balanced?")
	print(fm.IsMeasureBalancedMob(mea1mob,env)) 

	print("Is a standard measure selfdual?")
	print(fm.IsMeasureSelfdual(mea1,env))

	print("Is a Mobius measure selfdual?")
	print(fm.IsMeasureSelfdualMob(mea1mob,env))

	print("Is a standard measure subadditive?")
	print(fm.IsMeasureSubadditive(mea1,env))

	print("Is a Mobius measure subadditive?")
	print(fm.IsMeasureSubadditiveMob(mea1mob,env))

	print("Is a standard measure submodular?")
	print(fm.IsMeasureSubmodular(mea1,env))

	print("Is a Mobius measure submodular?")
	print(fm.IsMeasureSubmodularMob(mea1mob,env))

	print("Is a standard measure superadditive?")
	print(fm.IsMeasureSuperadditive(mea1,env))

	print("Is a Mobius measure superadditive?")
	print(fm.IsMeasureSuperadditiveMob(mea1mob,env))

	print("Is a standard measure supermodular?")
	print(fm.IsMeasureSupermodular(mea1,env))

	print("Is a Mobius measure supermodular?")
	print(fm.IsMeasureSupermodularMob(mea1mob,env))

	print("Is a standard measure symmetric?")
	print(fm.IsMeasureSymmetric(mea1,env))

	print("Is a Mobius measure symmetric?")
	print(fm.IsMeasureSymmetricMob(mea1mob,env))

	print("A standard measure is k-maxitive for k=")
	print(fm.IsMeasureKmaxitive (mea1,env))

	print("A Mobius measure is k-maxitive for k=")
	print(fm.IsMeasureKmaxitiveMob (mea1mob,env))


	
    print("Orness value of the Choquet integral for a standard fuzzy measure")
	print(fm.OrnessChoquet(mea1,env))

    print("Orness value of the Choquet integral for a Mobius fuzzy measure")
	print(fm.OrnessChoquetMob(mea1mob,env))

	print("Shapley value for standard representation")
	print(fm.Shapley(mea1,env))

	print("Shapley value for Mobius representation")
	print(fm.ShapleyMob(mea1mob,env))

	print("Sugeno integral from general fuzzy measure")
	print(fm.Sugeno(c(0.6, 0.3, 0.8), c(0, 0.3, 0.5, 0.6, 0.4, 0.8, 0.7, 1),env))

	print("Sugeno integral from Mobius fuzzy measure")
	print(fm.SugenoMob(c(0.6, 0.3, 0.8), c(0.0, 0.3, 0.5, -0.2, 0.4, 0.1, -0.2, 0.1),env)) 

	print("Zeta transform")
	print(fm.Zeta(mea1mob,env))
	
	print("Fitting a k-tolerant fuzzy measure to data")
	print(mea1<-fm.fittingKtolerant(d,env,2))
	
	print("Fitting a k-maxitive Mobius fuzzy measure to data")
	print(mea1<-fm.fittingKmaxitive(d,env,2))

	print("Fitting a k-interactive Mobius fuzzy measure to data")
	print(mea1<-fm.fittingKinteractive(d,env,2, 0.5))

	print("Fitting a k-interactive Mobius fuzzy measure to data automatically fitting K")
	print(mea1<-fm.fittingKinteractiveAuto(d,env,2))

	print("Fitting a k-interactive Mobius fuzzy measure to data using maximal chains method")
	print(mea1<-fm.fittingKinteractiveMC(d,env,2, 0.5))
}


fm.Zeta<- function(Mob,env)
{
	if(fm.errorcheck(env)) {
		print("Incorrect environment specified, call env<-fm.Init(n) first.");
		return (NULL);
	}
	if(env$m!=length(Mob)) {
		print("The environment mismatches the dimension to the fuzzy measure.");
		return (NULL);
	}

    # Calculates the general fuzzy measure from its Mobius representation.
    ZetaVal <- array(0.0,length(Mob));
    ZetaValue <- .C("ZetaCall", as.numeric(Mob), 
        out = as.numeric(ZetaVal),
        as.integer(env$n), 
	 as.integer(env$m), as.integer(env$card), as.integer(env$cardpos),as.double(env$bit2card),as.double(env$card2bit),as.double(env$factorials)
);
					
    return (ZetaValue$out);
}


fm.fittingWAM<- function(data, env=NULL)
{
	# This function estimates the values of a k-additive standard fuzzy measure based on empirical data. 
	# The result is an array containing the values of the fuzzy measure in Mobius, ordered according to set cardinalities.
	# kadd define the complexity of fuzzy measure. if kadd is not provided, its default value is equal to the number of inputs.

	size <- dim(as.matrix(data));
	n <- size[2] - 1;
	datanum <- size[1];
	WeightVal <- array(0.0,n);
 
	WeightValue <- .C("fittingWAMCall", as.integer(n),
	  				    as.integer(datanum),
			              out = as.numeric(WeightVal),
			                    as.numeric(t(data)));
					
	return (WeightValue$out);

}
fm.fittingOWA<- function(data, env=NULL)
{
	# This function estimates the values of a k-additive standard fuzzy measure based on empirical data. 
	# The result is an array containing the values of the fuzzy measure in Mobius, ordered according to set cardinalities.
	# kadd define the complexity of fuzzy measure. if kadd is not provided, its default value is equal to the number of inputs.

	size <- dim(as.matrix(data));
	n <- size[2] - 1;
	datanum <- size[1];
	WeightVal <- array(0.0,n);

	WeightValue <- .C("fittingOWACall", as.integer(n),
	  				    as.integer(datanum),
			              out = as.numeric(WeightVal),
			                    as.numeric(t(data)));
 					
	return (WeightValue$out);

}


fm.NonmodularityIndex <- function(v, env = NULL) {

    if (fm.errorcheck(env)) {
        print("Incorrect environment specified, call env<-fm.Init(n) first.");
        return(NULL);
    }
    if (env$m != length(v)) {
        print("The environment mismatches the dimension to the fuzzy measure.");
        return(NULL);
    }
    coalition <- array(0,length(v));
    Nonmodularityindexval <- array(0.0, length(v));
    # array of m zeros
    Nonmodularityindexvalue <- .C("NonmodularityIndexCall",
                                as.numeric(v),
                                out = as.numeric(Nonmodularityindexval),
    #as.integer(log2(length(v))),
                                as.integer(log2(length(v))),
                                as.integer(env$m), as.integer(env$card), as.integer(env$cardpos), as.double(env$bit2card), as.double(env$card2bit), as.double(env$factorials)

    );
	
	
    inteIndex <- as.matrix(Nonmodularityindexvalue$out);

 	Co <- .C("ShowCoalitionsCall", as.integer(env$m), coal = as.integer(coalition)); 
      coalIndex <- as.matrix(Co$coal);
    index <- cbind(inteIndex,coalIndex); 				
    return (round(index, digits=4));

}

fm.NonmodularityIndexMob <- function(Mob, env = NULL) {

    if (fm.errorcheck(env)) {
        print("Incorrect environment specified, call env<-fm.Init(n) first.");
        return(NULL);
    }

    if (env$m != length(Mob)) {
        print("The environment mismatches the dimension to the fuzzy measure.");
        return(NULL);
    }

    coalition <- array(0,length(Mob));
    NonmodularityindexMobval <- array(0.0, length(Mob));
    # array of m zeros
    NonmodularityindexMobvalue <- .C("NonmodularityIndexMobCall",
                                as.numeric(Mob),
                                out = as.numeric(NonmodularityindexMobval),
                                as.integer(log2(length(Mob))),
                                #as.integer(length(Mov)),
                                as.integer(env$m), as.integer(env$card), as.integer(env$cardpos), as.double(env$bit2card), as.double(env$card2bit), as.double(env$factorials)

    );
	
    inteIndex <- as.matrix(NonmodularityindexMobvalue$out);

 	Co <- .C("ShowCoalitionsCall", as.integer(env$m), coal = as.integer(coalition)); 
      coalIndex <- as.matrix(Co$coal);
    index <- cbind(inteIndex,coalIndex); 				
    return (round(index, digits=4));
	

}

fm.NonmodularityIndexMobkadditive <- function(Mob, env = NULL, kadd = "NA") {
      if (fm.errorcheck(env)) {
        print("Incorrect environment specified, call env<-fm.Init(n) first.");
        return(NULL);
    }

 #   if (env$m != length(Mob)) {  can be different
 #       print("The environment mismatches the dimension to the fuzzy measure.");
 #       return(NULL);
 #   }

    if (kadd == "NA") {
        kadd = env$n;
    }

    coalition <- array(0, env$m);
    NonmodularityIndexMobkadditiveVal <- array(0.0, env$m);
    NonmodularityIndexMobkadditiveValue <- .C("NonmodularityIndexMobkadditiveCall", as.numeric(Mob),
                           inter = as.numeric(NonmodularityIndexMobkadditiveVal),
                            as.integer(env$n),
    #                       as.integer(length(Mob)),
                           as.integer(kadd),
                           as.integer(env$m), as.integer(env$card), as.integer(env$cardpos), as.double(env$bit2card), as.double(env$card2bit), as.double(env$factorials)
    );
    inteIndex <- as.matrix(NonmodularityIndexMobkadditiveValue$inter);

    Co <- .C("ShowCoalitionsCall", as.integer(env$m), coal = as.integer(coalition));
    coalIndex <- as.matrix(Co$coal);
    index <- cbind(inteIndex, coalIndex);
    return(round(index, digits = 4));

}

fm.NonmodularityIndexKinteractive <- function(v, env = NULL, kadd = "NA") {
    if (fm.errorcheck(env)) {
        print("Incorrect environment specified, call env<-fm.Init(n) first.");
        return(NULL);
    }
 #   if (env$m != length(v)) {
 #       print("The environment mismatches the dimension to the fuzzy measure.");
 #       return(NULL);
 #   }
    if (kadd == "NA") {
        kadd = env$n;
    }

    coalition <- array(0, env$m);
    NonmodularityIndexKinteractiveVal <- array(0.0, env$m);
    NonmodularityIndexKinteractiveValue <- .C("NonmodularityIndexKinteractiveCall", as.numeric(v),
                                              inter = as.numeric(NonmodularityIndexKinteractiveVal),
                                              as.integer(env$n),
                                              as.integer(kadd),
                                              as.integer(env$m), as.integer(env$card), as.integer(env$cardpos), as.double(env$bit2card), as.double(env$card2bit), as.double(env$factorials)
    );
    inteIndex <- as.matrix(NonmodularityIndexKinteractiveValue$inter);

    Co <- .C("ShowCoalitionsCall", as.integer(env$m), coal = as.integer(coalition));
    coalIndex <- as.matrix(Co$coal);
    index <- cbind(inteIndex, coalIndex);
    return(round(index, digits = 4));
}


fm.ShowCoalitionsCard <- function(env = NULL) {
    
    if (fm.errorcheck(env)) {
        print("Incorrect environment specified, call env<-fm.Init(n) first.");
        return(NULL);
    }

    coalition <- array(0, env$m);

    Co <- .C("ShowCoalitionsCardCall", as.integer(env$m), coal = as.integer(coalition), as.double(env$card2bit));
    coalIndex <- as.matrix(Co$coal);
    return(coalIndex);

}

fm.ShowCoalitions <- function(env = NULL) {
     if (fm.errorcheck(env)) {
        print("Incorrect environment specified, call env<-fm.Init(n) first.");
        return(NULL);
    }

    coalition <- array(0, env$m);

    Co <- .C("ShowCoalitionsCall", as.integer(env$m), coal = as.integer(coalition));
    coalIndex <- as.matrix(Co$coal);
    return(coalIndex);
}

fm.dualMobKadd <- function(Mob, env = NULL, kadd = "NA") {
        # Calculates the dual of kadditive fuzzy measure Mob in Mobius representation, returns it as value of the function (array of size Mob in cardinality ordering).
        if (fm.errorcheck(env)) {
            print("Incorrect environment specified, call env<-fm.Init(n) first.");
            return(NULL);
        }

        if (kadd == "NA") {
            kadd = env$n;
        }
        
        dualMobKaddVal <- array(0.0, length(Mob));
        # array of m zeros
        dualMobKaddValue <- .C("dualMobKaddCall",
                           as.integer(env$n),   #should be n, not m
                           as.integer(length(Mob)),
                           as.integer(kadd),
                           as.numeric(Mob),
                           out = as.numeric(dualMobKaddVal),
                           as.integer(env$card), as.integer(env$cardpos), as.double(env$bit2card), as.double(env$card2bit), as.double(env$factorials)
        );
        return(dualMobKaddValue$out);
    }

fm.fm_arraysize <- function( env = NULL, kint = "NA") {
        
        if (fm.errorcheck(env)) {
            print("Incorrect environment specified, call env<-fm.Init(n) first.");
            return(NULL);
        }

        if (kint == "NA") {
            kint = env$n;
        }
    if (kint <= 0 | kint > env$n) {
        print("Incorrect argument kint");
        return(NULL);
    }
      outval=0;
        fm_arraysizeValue <- .C("fm_arraysizeCallR",
                            as.integer(env$n),
                            as.integer(env$m), as.integer(kint), out = as.integer(outval), as.double(env$factorials)
        );
    return(fm_arraysizeValue$out);
   }
fm.fm_arraysizekadd <- function( env = NULL, kint = "NA") {
        
        if (fm.errorcheck(env)) {
            print("Incorrect environment specified, call env<-fm.Init(n) first.");
            return(NULL);
        }

        if (kint == "NA") {
            kint = env$n;
        }
    if (kint <= 0 | kint > env$n) {
        print("Incorrect argument kint");
        return(NULL);
    }
      outval=0;
        fm_arraysizeValue <- .C("fm_arraysizekaddCall",
                            as.integer(env$n),
                            as.integer(env$m), as.integer(kint), out = as.integer(outval), as.double(env$factorials)
        );
     
    return(fm_arraysizeValue$out);
   }



fm.generate_fm_tsort <- function( num, kint, markov, option, K, env = NULL) {
        # Calculates the dual of fuzzy measure v, returns it as value of the function (array of size m).
        if (fm.errorcheck(env)) {
            print("Incorrect environment specified, call env<-fm.Init(n) first.");
            return(NULL);
        }

        if (num <= 0) {
            print("Incorrect argument num");
            return(NULL);
        }



        if (markov <= 0) {
            print("Incorrect argument markov");
            return(NULL);
        }

        if (option < 0) {
            print("Incorrect argument option");
            return(NULL);
        }

        if (K == "NA") {
            K = 1;
        }
        if (kint == "NA") {
            kint = env$n;
        }
        if (kint <= 0 | kint > env$n) {
            print("Incorrect argument kint");
            return(NULL);
        }

        generate_fm_tsortVal <- array(0.0, num * env$m);
        # array of m zeros
        generate_fm_tsortValue <- .C("generate_fm_tsortCall",
                                 as.integer(num), as.integer(env$n), as.integer(kint), as.integer(markov), as.integer(option), as.numeric(K),
                                 out = as.numeric(generate_fm_tsortVal),
                                 as.integer(env$card), as.integer(env$cardpos), as.double(env$bit2card), as.double(env$card2bit), as.double(env$factorials)

        );
        return(generate_fm_tsortValue$out);
    }

fm.generate_fmconvex_tsort <- function(num, kint, markov, option, K, env = NULL) {
    # Calculates the dual of fuzzy measure v, returns it as value of the function (array of size m).
    if (fm.errorcheck(env)) {
        print("Incorrect environment specified, call env<-fm.Init(n) first.");
        return(NULL);
    }
    if (num <= 0) {
        print("Incorrect argument num");
        return(NULL);
    }




    if (markov <= 0) {
        print("Incorrect argument markov");
        return(NULL);
    }

    if (option < 0) {
        print("Incorrect argument option");
        return(NULL);
    }

    if (K == "NA") {
        K = 1;
    }
    if (kint == "NA") {
        kint = env$n;
    }
    if (kint <= 0 | kint > env$n) {
        print("Incorrect argument kint");
        return(NULL);
    }
    generate_fm_tsortVal <- array(0.0, num * env$m);
    # array of m zeros
    generate_fm_tsortValue <- .C("generate_fmconvex_tsortCall",
                                 as.integer(num), as.integer(env$n), as.integer(kint), as.integer(markov), as.integer(option), as.numeric(K),
                                 out = as.numeric(generate_fm_tsortVal),
                                 as.integer(env$card), as.integer(env$cardpos), as.double(env$bit2card), as.double(env$card2bit), as.double(env$factorials)

        );
    return(generate_fm_tsortValue$out);
}

fm.generate_fm_minplus <- function(num, kint, markov, option, K, env = NULL) {
    # 
    if (fm.errorcheck(env)) {
        print("Incorrect environment specified, call env<-fm.Init(n) first.");
        return(NULL);
    }
    if (num <= 0) {
        print("Incorrect argument num");
        return(NULL);
    }

 

    if (markov <= 0) {
        print("Incorrect argument markov");
        return(NULL);
    }

    if (option < 0) {
        print("Incorrect argument option");
        return(NULL);
    }

    if (K == "NA") {
        K = 1;
    }
    if (kint == "NA") {
        kint = env$n;
    }
   if (kint <= 0 | kint > env$n) {
        print("Incorrect argument kint");
        return(NULL);
    }

    generate_fm_tsortVal <- double( num * env$m);
    # array of m zeros
    generate_fm_tsortValue <- .C("generate_fm_minplusCall",
                                 as.integer(num), as.integer(env$n), as.integer(kint), as.integer(markov), as.integer(option), as.numeric(K),
                                 out = as.numeric(generate_fm_tsortVal),
                                 as.integer(env$card), as.integer(env$cardpos), as.double(env$bit2card), as.double(env$card2bit), as.double(env$factorials)

        );
    return(generate_fm_tsortValue$out);
}



    fm.export_maximal_chains <- function(v, env = NULL) {
        # 
        if (fm.errorcheck(env)) {
            print("Incorrect environment specified, call env<-fm.Init(n) first.");
            return(NULL);
        }
        if (env$m != length(v)) {
            print("The environment mismatches the dimension to the fuzzy measure.");
            return(NULL);
        }

        export_maximal_chainsVal <- array(0.0, env$n * factorial(env$n) );
        # array of m zeros
        export_maximal_chainsValue <- .C("export_maximal_chainsCall",
                                     as.integer(env$n),
                                     as.numeric(v),
                                     out = as.numeric(export_maximal_chainsVal),
                                     as.double(env$factorials)
        );
        return(export_maximal_chainsValue$out);
    }





## Sparse FM representation ======================================##
fm.tuple_cardinality_sparse <- function(i, envsp = NULL) {

    if (fm.errorchecksparse(envsp)) {
        print("Incorrect environment specified, call env<-fm.PrepareSparseFM first.");
        return(NULL);
    }

    tuple_cardinality_sparseVal <- 0
	
	tuple_cardinality_sparseValue <- .C("tuple_cardinality_sparseCall", as.integer(i),
                                out=as.integer(tuple_cardinality_sparseVal),
								 as.integer(envsp$n),  as.double(envsp$singletons),
								as.double(envsp$pairs), as.double(envsp$tuples), as.integer(envsp$pairsidx), 
								as.integer(envsp$tuplesidx), as.integer(envsp$tuplescon), as.integer(envsp$dims) ,NAOK=TRUE  );	
	
    return(tuple_cardinality_sparseValue$out);	
}

fm.get_num_tuples <- function(envsp = NULL) {

    if (fm.errorchecksparse(envsp)) {
        print("Incorrect environment specified, call env<-fm.PrepareSparseFM first.");
        return(NULL);
    }
       
    get_num_tuplesVal <- 0;
	
	get_num_tuplesValue <- .C("get_num_tuplesCall",
                                out=as.integer(get_num_tuplesVal),
								 as.integer(envsp$n),  as.double(envsp$singletons),
								as.double(envsp$pairs), as.double(envsp$tuples), as.integer(envsp$pairsidx), 
								as.integer(envsp$tuplesidx), as.integer(envsp$tuplescon), as.integer(envsp$dims) ,NAOK=TRUE );	
	
    return(get_num_tuplesValue$out);
}

fm.get_sizearray_tuples <- function(envsp = NULL) {

    if (fm.errorchecksparse(envsp)) {
        print("Incorrect environment specified, call env<-fm.PrepareSparseFM first.");
        return(NULL);
    }

    get_sizearray_tuplesVal <-0;
 
	get_sizearray_tuplesValue <- .C("get_sizearray_tuplesCall",
                                out=as.integer(get_sizearray_tuplesVal),
								 as.integer(envsp$n),  as.double(envsp$singletons),
								as.double(envsp$pairs), as.double(envsp$tuples), as.integer(envsp$pairsidx), 
								as.integer(envsp$tuplesidx), as.integer(envsp$tuplescon), as.integer(envsp$dims) ,NAOK=TRUE  );	
	
    return(get_sizearray_tuplesValue$out);
}



fm.is_inset_sparse <- function( A, card, i, envsp = NULL) {

    if (fm.errorchecksparse(envsp)) {
        print("Incorrect environment specified, call env<-fm.PrepareSparseFM first.");
        return(NULL);
    }

    is_inset_sparseVal <- 0;
	is_inset_sparseValue <- .C("is_inset_sparseCall",
                                 as.integer(A), as.integer(card), as.integer(i),  out=as.integer(is_inset_sparseVal),
								 as.integer(envsp$n),  as.double(envsp$singletons),
								as.double(envsp$pairs), as.double(envsp$tuples), as.integer(envsp$pairsidx), 
								as.integer(envsp$tuplesidx), as.integer(envsp$tuplescon), as.integer(envsp$dims)  ,NAOK=TRUE );

    return(as.logical(is_inset_sparseValue$out))
}


fm.is_subset_sparse <- function(A, cardA, B, cardB, envsp = NULL) {

    if (fm.errorchecksparse(envsp)) {
        print("Incorrect environment specified, call env<-fm.PrepareSparseFM first.");
        return(NULL);
    }

    is_subset_sparseVal <- 0;
	
	is_subset_sparseValue <- .C("is_subset_sparseCall",
                                 as.integer(A), as.integer(cardA), as.integer(B), as.integer(cardB), out=as.integer(is_subset_sparseVal),
								 as.integer(envsp$n),  as.double(envsp$singletons),
								as.double(envsp$pairs), as.double(envsp$tuples), as.integer(envsp$pairsidx), 
								as.integer(envsp$tuplesidx), as.integer(envsp$tuplescon), as.integer(envsp$dims) ,NAOK=TRUE  );

    return(as.logical(is_subset_sparseValue$out))
}

fm.min_subset_sparse <- function(x, S, cardS, envsp = NULL) {

    if (fm.errorchecksparse(envsp)) {
        print("Incorrect environment specified, call env<-fm.PrepareSparseFM first.");
        return(NULL);
    }

    min_subset_sparseVal <- 0.0;
	
		
	min_subset_sparseValue <- .C("min_subset_sparseCall",
                                 as.numeric(x), as.integer(S), as.integer(cardS), out=as.numeric(min_subset_sparseVal),
								 as.integer(envsp$n),  as.double(envsp$singletons),
								as.double(envsp$pairs), as.double(envsp$tuples), as.integer(envsp$pairsidx), 
								as.integer(envsp$tuplesidx), as.integer(envsp$tuplescon), as.integer(envsp$dims)  ,NAOK=TRUE );

    return(min_subset_sparseValue$out);
}

fm.max_subset_sparse <- function(x, S, cardS, envsp = NULL) {

    if (fm.errorchecksparse(envsp)) {
        print("Incorrect environment specified, call env<-fm.PrepareSparseFM first.");
        return(NULL);
    }

    max_subset_sparseVal <- 0.0;
	
	max_subset_sparseValue <- .C("max_subset_sparseCall",
                                 as.numeric(x), as.integer(S), as.integer(cardS), out=as.numeric(max_subset_sparseVal),
								 as.integer(envsp$n),  as.double(envsp$singletons),
								as.double(envsp$pairs), as.double(envsp$tuples), as.integer(envsp$pairsidx), 
								as.integer(envsp$tuplesidx), as.integer(envsp$tuplescon), as.integer(envsp$dims) ,NAOK=TRUE  );

    return(max_subset_sparseValue$out);
}


  
fm.ChoquetMob_sparse <- function(x, envsp = NULL) {

    if (fm.errorchecksparse(envsp)) {
        print("Incorrect environment specified, call env<-fm.PrepareSparseFM first.");
        return(NULL);
    }

    ChoquetMob_sparseVal <- 0.0;

    ChoquetMob_sparseValue <- .C("ChoquetMob_sparseCall",
                                 as.numeric(x), out=as.numeric(ChoquetMob_sparseVal),
								 as.integer(envsp$n),  as.double(envsp$singletons),
								as.double(envsp$pairs), as.double(envsp$tuples), as.integer(envsp$pairsidx), 
								as.integer(envsp$tuplesidx), as.integer(envsp$tuplescon), as.integer(envsp$dims) ,NAOK=TRUE  );
    return(ChoquetMob_sparseValue$out);
}

fm.ShapleyMob_sparse <- function(n, envsp = NULL) {

    if (fm.errorchecksparse(envsp)) {
        print("Incorrect environment specified, call env<-fm.PrepareSparseFM first.");
        return(NULL);
    }

    ShapleyVal <- array(0.0, n);
    ShapleyValue <- .C("ShapleyMobsparse_Call",
                                  out=as.numeric(ShapleyVal),
								 as.integer(envsp$n),  as.double(envsp$singletons),
								as.double(envsp$pairs), as.double(envsp$tuples), as.integer(envsp$pairsidx), 
								as.integer(envsp$tuplesidx), as.integer(envsp$tuplescon), as.integer(envsp$dims) ,NAOK=TRUE  );
    return(round(ShapleyValue$out, digits = 4));
}

fm.BanzhafMob_sparse <- function(n, envsp = NULL) {

    if (fm.errorchecksparse(envsp)) {
        print("Incorrect environment specified, call env<-fm.PrepareSparseFM first.");
        return(NULL);
    }
	
	
    BanzhafMob_sparseVal <- array(0.0, n);
    BanzhafMob_sparseValue <- .C("BanzhafMob_sparseCall",
                                  out=as.numeric(BanzhafMob_sparseVal),
								 as.integer(envsp$n),  as.double(envsp$singletons),
								as.double(envsp$pairs), as.double(envsp$tuples), as.integer(envsp$pairsidx), 
								as.integer(envsp$tuplesidx), as.integer(envsp$tuplescon), as.integer(envsp$dims) ,NAOK=TRUE  );
    return(round(BanzhafMob_sparseValue$out, digits = 4));
}


fm.add_singletons_sparse <- function(v, envsp = NULL) {

    if (fm.errorchecksparse(envsp)) {
        print("Incorrect environment specified, call env<-fm.PrepareSparseFM first.");
        return(NULL);
    }
	
#check if singletons is updated
    add_singletons_sparseValue <- .C("add_singletons_sparseCall",
                                    as.numeric(v), as.integer(envsp$n), out=as.double(envsp$singletons),NAOK=TRUE );
									

  envsp$singletons=add_singletons_sparseValue$out;
  return(envsp);

}


fm.add_pair_sparse <- function( i, j, v, envsp = NULL) {
### this can be done in a more efficient way just here in R , augmenting the arrays like in C code

    if (fm.errorchecksparse(envsp)) {
        print("Incorrect environment specified, call env<-fm.PrepareSparseFM first.");
        return(NULL);
    }

	if(length(envsp$pairs)<=envsp$dim[1] ) { length(envsp$pairs)<- length(envsp$pairs)*2 ; 
	            length(envsp$pairsidx)<- length(envsp$pairsidx)*2; }
	
	#augment the space reserved
	
	# now how do we return? again allocate space
	add_pair_sparseValue <- .C("add_pair_sparseCall", as.integer(i), as.integer(j), as.numeric(v) ,
								as.integer(envsp$n),  as.double(envsp$singletons),
								pairs=as.double(envsp$pairs), as.double(envsp$tuples), pairsidx=as.integer(envsp$pairsidx), 
								as.integer(envsp$tuplesidx), as.integer(envsp$tuplescon), dims=as.integer(envsp$dims) ,NAOK=TRUE   );
								
  envsp$pairs=add_pair_sparseValue$pairs;
  envsp$pairsidx=add_pair_sparseValue$pairsidx;
  envsp$dims=add_pair_sparseValue$dims;
  return(envsp);								
								
#  out<- .C("Prepare_FM_sparseCall",n=as.integer(n), as.integer(tupsz), as.double(tup), as.integer(tupidx), singletons=as.double(1:n),
#  pairs=as.double(1:tupsz), tuples=as.double(1:tupsz), pairsidx=as.integer(1:2*tupsz), 
#  tuplesidx=as.integer(1:tupsz*2), tuplescon=as.integer(1:tupidx),
#  dims=as.integer(1:4)								

}

fm.add_tuple_sparse <- function( tuple, v, envsp = NULL) {

    if (fm.errorchecksparse(envsp)) {
        print("Incorrect environment specified, call env<-fm.PrepareSparseFM first.");
        return(NULL);
    }
	
	tupsize=length(tuple);
	if(tupsize>envsp$n){
		print("Incorrect tuple, larger than n");
		return(envsp);
	}
	
	if(length(envsp$tuples)<=envsp$dim[2] ) { length(envsp$tuples)<- (length(envsp$tuples)+1)*2 ; 
	            length(envsp$tuplesidx)<- (length(envsp$tuplesidx)+1)*2; }
	if(length(envsp$tuplescon)<=envsp$dim[4]+envsp$n+1 ) { length(envsp$tuplescon)<- (length(envsp$tuplescon)+1)*2  }
				
	#print((envsp$tuplescon))
	#augment the space reserved	
		add_tuple_sparseValue <- .C("add_tuple_sparseCall", as.integer(tupsize), as.integer(tuple), as.numeric(v) ,
								as.integer(envsp$n),  as.double(envsp$singletons),
								as.double(envsp$pairs), tuples=as.double(envsp$tuples), as.integer(envsp$pairsidx), 
								tuplesidx=as.integer(envsp$tuplesidx), tuplescon=as.integer(envsp$tuplescon), dims=as.integer(envsp$dims),NAOK=TRUE  );
								
	  envsp$tuples=add_tuple_sparseValue$tuples;
	  envsp$tuplesidx=add_tuple_sparseValue$tuplesidx;	  
	  envsp$tuplescon=add_tuple_sparseValue$tuplescon;	  
	  envsp$dims=add_tuple_sparseValue$dims;
      return(envsp);								
}

fm.populate_fm_2add_sparse <- function(singletons, numpairs, pairs, indicesp1, indicesp2, envsp = NULL) {

    if (fm.errorchecksparse(envsp)) {
        print("Incorrect environment specified, call env<-fm.PrepareSparseFM first.");
        return(NULL);
    }

    if (numpairs == "NA") {
        numpairs = 0;
    }

	envsp$dims=c(0,0, 0,0)
	envsp$pairs=double(numpairs)
	envsp$pairsidx=integer(numpairs*2)	
	
  #  print(envsp)
    
	
	populate_fm_2add_sparseValue <- .C("populate_fm_2add_sparseCall", as.double(singletons), 
			as.integer(numpairs), as.double(pairs), as.integer(indicesp1), as.integer(indicesp2),
								as.integer(envsp$n),  singletons1=as.double(envsp$singletons),
								pairs1=as.double(envsp$pairs), tuples=as.double(envsp$tuples), pairsidx=as.integer(envsp$pairsidx),
								tuplesidx=as.integer(envsp$tuplesidx), tuplescon=as.integer(envsp$tuplescon), dims=as.integer(envsp$dims) ,NAOK=TRUE );
#	print("exit");
    
	  envsp$singletons=populate_fm_2add_sparseValue$singletons1;
	  envsp$pairs=populate_fm_2add_sparseValue$pairs1;
	  envsp$pairsidx=populate_fm_2add_sparseValue$pairsidx;
	  envsp$tuples=populate_fm_2add_sparseValue$tuples;
	  envsp$tuplesidx=populate_fm_2add_sparseValue$tuplesidx;	  
	  envsp$tuplescon=populate_fm_2add_sparseValue$tuplescon;	  
	  envsp$dims=populate_fm_2add_sparseValue$dims;
#      print("check1");
      print(envsp$dims)
      return(envsp);
}

fm.populate_fm_2add_sparse_from2add <- function(n, v, envsp = NULL) {

    if (fm.errorchecksparse(envsp)) {
        print("Incorrect environment specified, call env<-fm.PrepareSparseFM first.");
        return(NULL);
    }

	envsp$dims=c(0, 0, 0,0)
	envsp$pairs=double(envsp$n**2 /2)
	envsp$pairsidx=integer(envsp$n**2)
								
	populate_fm_2add_sparseValue <- .C("populate_fm_2add_sparse_from2addCall",  
								as.integer(envsp$n),  as.double(v),  singletons=as.double(envsp$singletons),
								pairs=as.double(envsp$pairs), tuples=as.double(envsp$tuples), pairsidx=as.integer(envsp$pairsidx), 
								tuplesidx=as.integer(envsp$tuplesidx), tuplescon=as.integer(envsp$tuplescon), dims=as.integer(envsp$dims) ,NAOK=TRUE );

		envsp$n=n;
	  envsp$singletons=populate_fm_2add_sparseValue$singletons;
	  envsp$pairs=populate_fm_2add_sparseValue$pairs;
	  envsp$pairsidx=populate_fm_2add_sparseValue$pairsidx;	
	  envsp$tuples=populate_fm_2add_sparseValue$tuples;
	  envsp$tuplesidx=populate_fm_2add_sparseValue$tuplesidx;	  
	  envsp$tuplescon=populate_fm_2add_sparseValue$tuplescon;	  
	  envsp$dims=populate_fm_2add_sparseValue$dims;
      return(envsp);								
}


fm.expand_2add_full <- function(n, envsp = NULL) {

    if (fm.errorchecksparse(envsp)) {
        print("Incorrect environment specified, call env<-fm.PrepareSparseFM first.");
        return(NULL);
    }

    expand_2add_fullVal <- array(0.0, n * (n - 1) / 2 + n);
	
	expand_2add_fullValue <- .C("expand_2add_fullCall", out=as.numeric(expand_2add_fullVal), 
								as.integer(envsp$n),  as.double(envsp$singletons),
								as.double(envsp$pairs), as.double(envsp$tuples), as.integer(envsp$pairsidx), 
								as.integer(envsp$tuplesidx), as.integer(envsp$tuplescon), as.integer(envsp$dims) ,NAOK=TRUE );
	
    # array of pairs and singletons 
    return(expand_2add_fullValue$out);
}

fm.expand_sparse_full <- function(n, envsp = NULL) {

    if (fm.errorchecksparse(envsp)) {
        print("Incorrect environment specified, call env<-fm.PrepareSparseFM first.");
        return(NULL);
    }
	Val <- array(0.0, 2**n);
	return ( .C("expand_sparse_fullCall", out=as.numeric(Val), 
								as.integer(envsp$n),  as.double(envsp$singletons),
								as.double(envsp$pairs), as.double(envsp$tuples), as.integer(envsp$pairsidx), 
								as.integer(envsp$tuplesidx), as.integer(envsp$tuplescon), as.integer(envsp$dims) ,NAOK=TRUE )$out);
						
 #   return(expand_sparse_fullVal$out);
}

fm.sparse_get_singletons <- function( envsp = NULL) {

    if (fm.errorchecksparse(envsp)) {
        print("Incorrect environment specified, call env<-fm.PrepareSparseFM first.");
        return(NULL);
    }
	return ( .C("sparse_get_singletonsCall", out=double(envsp$n), 
								as.integer(envsp$n),  as.double(envsp$singletons),
								as.double(envsp$pairs), as.double(envsp$tuples), as.integer(envsp$pairsidx), 
								as.integer(envsp$tuplesidx), as.integer(envsp$tuplescon), as.integer(envsp$dims) ,NAOK=TRUE )$out);	
}


fm.sparse_get_pairs <- function(envsp = NULL) {

    if (fm.errorchecksparse(envsp)) {
        print("Incorrect environment specified, call env<-fm.Init(n) first.");
        return(NULL);
    }
    siz<-0;
	sparse_get_pairsValue<-.C("sparse_get_pairsCall", outidx=integer(2*envsp$dim[1]), out=double(envsp$dim[1]), sz=as.integer(siz),
								as.integer(envsp$n),  as.double(envsp$singletons),
								as.double(envsp$pairs), as.double(envsp$tuples), as.integer(envsp$pairsidx), 
								as.integer(envsp$tuplesidx), as.integer(envsp$tuplescon), as.integer(envsp$dims) ,NAOK=TRUE );	

    #cut the arrays to sz
#    length(sparse_get_pairsValue$out) <- sparse_get_pairsValue$sz;
#    length(sparse_get_pairsValue$outIdx) <- sparse_get_pairsValue$sz*2;

    return(list(sparse_get_pairsValue$out, sparse_get_pairsValue$outidx));
}

fm.sparse_get_tuples <- function( envsp = NULL) {

    if (fm.errorchecksparse(envsp)) {
        print("Incorrect environment specified, call env<-fm.PrepareSparseFM first.");
        return(NULL);
    }

	siz<-0;
	sparse_get_tuplesValue<-.C("sparse_get_tuplesCall", outidx=integer(envsp$dim[4]), out=double(envsp$dim[2]), sz=as.integer(siz),
								as.integer(envsp$n),  as.double(envsp$singletons),
								as.double(envsp$pairs), as.double(envsp$tuples), as.integer(envsp$pairsidx), 
								as.integer(envsp$tuplesidx), as.integer(envsp$tuplescon), as.integer(envsp$dims) ,NAOK=TRUE );	
								
    return(list(sparse_get_tuplesValue$out, sparse_get_tuplesValue$outidx));
}


fm.Shapley2addMob <- function(n, Mob) {
    Shapley2addMobVal <- array(0.0, n);

    Shapley2addMobValue <- .C("Shapley2addMobCall",
                              as.numeric(Mob),
                              out = as.numeric(Shapley2addMobVal),
                               as.integer(n)
    );
    return(round(Shapley2addMobValue$out, digits = 4));
}


fm.Banzhaf2addMob <- function(n, Mob) {
    Banzhaf2addMobVal <- array(0.0, n);

    Banzhaf2addMobValue <- .C("Banzhaf2addMobCall",
                               as.numeric(Mob),
                               out = as.numeric(Banzhaf2addMobVal),
                               as.integer(n)
    );
    return(round(Banzhaf2addMobValue$out, digits = 4));
}


fm.Choquet2addMob <- function(n, x, Mob) {
    Choquet2addMobVal <- 0.0;

    Choquet2addMobValue <- .C("Choquet2addMobCall",
                              as.numeric(Mob),
                              as.numeric(x),
                              out = as.numeric(Choquet2addMobVal),
                              as.integer(n)
    );
    return(Choquet2addMobValue$out);
}


fm.generate_fm_2additive_convex_sparse <- function(n, envsp = NULL) {

    if (fm.errorchecksparse(envsp)) {
        print("Incorrect environment specified, call env<-fm.PrepareSparseFM first.");
        return(NULL);
    }

	t=1
	envsp$n=n
	envsp$dims=c(0, 0, 0,0)
	envsp$pairs=double(envsp$n*(envsp$n-1) /2)
	envsp$pairsidx=integer(envsp$n**2)	
	envsp$dims=c(0, 0, 0,0)
								
	generate_fm_2additive <- .C("generate_fm_2additive_convex_sparseCall",  
								as.integer(envsp$n),  as.integer(t),  singletons=as.double(envsp$singletons),
								pairs=as.double(envsp$pairs), tuples=as.double(envsp$tuples), pairsidx=as.integer(envsp$pairsidx), 
								tuplesidx=as.integer(envsp$tuplesidx), tuplescon=as.integer(envsp$tuplescon), dims=as.integer(envsp$dims),NAOK=TRUE  );

	  envsp$n=n;
	  envsp$singletons=generate_fm_2additive$singletons;
	  envsp$pairs=generate_fm_2additive$pairs;
	  envsp$pairsidx=generate_fm_2additive$pairsidx;	
	  envsp$tuples=generate_fm_2additive$tuples;
	  envsp$tuplesidx=generate_fm_2additive$tuplesidx;	  
	  envsp$tuplescon=generate_fm_2additive$tuplescon;	  
	  envsp$dims=generate_fm_2additive$dims;
      return(envsp);						
}

fm.generate_fm_kadditive_convex_sparse <- function(n, kadd, nonzero, envsp = NULL) {
    #define nonzero
    if (fm.errorchecksparse(envsp)) {
        print("Incorrect environment specified, call env<-fm.PrepareSparseFM first.");
        return(NULL);
    }

    if (kadd == "NA") {
        kadd = n;
    }

	t=1
	envsp$n=n
#	envsp$dims=c(nonzero, nonzero, nonzero,nonzero*n)
	envsp$dims=c(0,0,0,0)
	envsp$pairs=double(nonzero)
	envsp$pairsidx=integer(nonzero*2)	
	envsp$tuples=double(nonzero)
	envsp$tuplesidx=integer(nonzero)	
	envsp$tuplescon=integer(nonzero*n)	
								
	generate_fm_2additive <- .C("generate_fm_kadditive_convex_sparseCall",  
								as.integer(envsp$n),  as.integer(kadd), as.integer(nonzero), as.integer(t), 
								singletons=as.double(envsp$singletons),
								pairs=as.double(envsp$pairs), tuples=as.double(envsp$tuples), pairsidx=as.integer(envsp$pairsidx), 
								tuplesidx=as.integer(envsp$tuplesidx), tuplescon=as.integer(envsp$tuplescon), dims=as.integer(envsp$dims)  );

	  envsp$n=n;
	  envsp$singletons=generate_fm_2additive$singletons;
	  envsp$pairs=generate_fm_2additive$pairs;
	  envsp$pairsidx=generate_fm_2additive$pairsidx;	
	  envsp$tuples=generate_fm_2additive$tuples;
	  envsp$tuplesidx=generate_fm_2additive$tuplesidx;	  
	  envsp$tuplescon=generate_fm_2additive$tuplescon;	  
	  envsp$dims=generate_fm_2additive$dims;
      return(envsp);			

}

fm.NonmodularityIndex_sparse <- function(n, envsp = NULL) {

#	out<-Nonmodularityindex_sparseCallcpp11(envsp);
#    return (out);

    if (fm.errorchecksparse(envsp)) {
        print("Incorrect environment specified, call env<-fm.PrepareSparseFM first.");
        return(NULL);
    }

    Nonmodularityindex_sparseVal <- array(0.0, 2**n);
    # array of m zeros
	
		return( .C("Nonmodularityindex_sparseCall", out = as.numeric(Nonmodularityindex_sparseVal),
								as.integer(envsp$n),  as.double(envsp$singletons),
								as.double(envsp$pairs), as.double(envsp$tuples), as.integer(envsp$pairsidx), 
								as.integer(envsp$tuplesidx), as.integer(envsp$tuplescon), as.integer(envsp$dims)  ,NAOK=TRUE)$out);
					
#    return(Nonmodularityindex_sparseValue$out);
}



fm.generate_fm_2additive_convex <- function( num, n) {
       
    generate_fm_2additive_convexVal <- ( (n * (n - 1) / 2 + n) * num);
	Val <- array(0.0, generate_fm_2additive_convexVal);
	size<-1;
    generate_fm_2additive_convexValue <- .C("generate_fm_2additive_convexCall",
                                          as.integer(num),
                                          as.integer(n),
                                          sz = as.integer(size),
                                          out = as.numeric(Val) );
	return(list(V=generate_fm_2additive_convexValue$out, len=generate_fm_2additive_convexValue$sz));
  #  return(rowr::cbind.fill(generate_fm_2additive_convexValue$out, generate_fm_2additive_convexValue$sz, fill = NA));
    #return(cbind(generate_fm_2additive_convexValue$out, generate_fm_2additive_convexValue$sz));
}

fm.generate_fm_2additive_concave <- function(num, n) {

    generate_fm_2additive_convexVal <- ( (n * (n - 1) / 2 + n) * num);
	
	Val <- array(0.0, generate_fm_2additive_convexVal);
	size<-1;
    generate_fm_2additive_convexValue <- .C("generate_fm_2additive_concaveCall",
                                          as.integer(num),
                                          as.integer(n),
                                          sz = as.integer(size),
                                          out = as.numeric(Val) );
		return(list(V=generate_fm_2additive_convexValue$out, len=generate_fm_2additive_convexValue$sz));
#    return(rowr::cbind.fill(generate_fm_2additive_convexValue$out, generate_fm_2additive_convexValue$sz, fill = NA));
    #return(cbind(generate_fm_2additive_convexValue$out, generate_fm_2additive_convexValue$sz));
}

fm.generate_fm_2additive_convex_withsomeindependent <- function( num, n) {
       
    generate_fm_2additive_convex_withsomeindependentVal <- ( (n * (n - 1) / 2 + n) * num*2);
	size<-1;
	Val <- array(0.0,  generate_fm_2additive_convex_withsomeindependentVal);
    outvalue <- .C("generate_fm_2additive_convex_withsomeindependentCall",
                                                              as.integer(num),
                                                              as.integer(n),
                                                              sz=as.integer(size),
                                                              out = as.numeric(Val)  );
		return(list(V=outvalue$out,len=outvalue$sz));
 #   return(rowr::cbind.fill(outvalue$out, outvalue$sz, fill = NA));
   # return(cbind(outvalue$out, outvalue$sz));
}

fm.generate_fm_2additive <- function( num, n) {
       #returns values and size of each
    sz <- ( (n * (n - 1) / 2 + n) );
	size<-1;
	Val <- array(0.0,  sz*num);
    outvalue <- .C("generate_fm_2additiveCall",
                                                              as.integer(num),
                                                              as.integer(n),
                                                              out = as.numeric(Val),
															  out1=as.integer(size)  );
		return(list(V=outvalue$out, len=outvalue$out1));
}


fm.generate_fm_sorting <- function(num, markov, option, env=NULL) {

    if (fm.errorcheck(env)) {
        print("Incorrect environment specified, call env<-fm.Init(n) first.");
        return(NULL);
    }
    if (num <= 0) {
        print("Incorrect argument num");
        return(NULL);
    }

 

    if (markov <= 0) {
        print("Incorrect argument markov");
        return(NULL);
    }

    if (option < 0) {
        print("Incorrect argument option");
        return(NULL);
    }


	generate_fm_sortingVal <- array(0.0, num * env$m);
	
    generate_fm_sortingValue <- .C("generate_fm_sortingCall",
                                          as.integer(num),
                                          as.integer(env$n),
                                          as.integer(markov),
										  as.integer(option),
                                          out = as.numeric(generate_fm_sortingVal) ,
										  as.integer(env$m), as.integer(env$card), as.integer(env$cardpos),as.double(env$bit2card),as.double(env$card2bit),as.double(env$factorials)
										  );
	return(generate_fm_sortingValue$out);									  
}

fm.generate_balanced <- function( num,  env=NULL) {
# returns how many or just the list
    if (fm.errorcheck(env)) {
        print("Incorrect environment specified, call env<-fm.Init(n) first.");
        return(NULL);
    }
    if (num <= 0) {
        print("Incorrect argument num");
        return(NULL);
    }


	Val <- array(0.0, num* env$m);
	t<-1;
    Res <- .C("generate_fm_balancedCall",
										  as.integer(num),
                                          as.integer(env$n),
                                          out = as.numeric(Val) ,
										  out1=as.integer(t),
										  as.integer(env$m), as.integer(env$card), as.integer(env$cardpos),as.double(env$bit2card),as.double(env$card2bit),as.double(env$factorials)
										  
										  );
	return(Res$out);									  
}

fm.generate_belief <- function( num, kadd, env=NULL) {
# returns how many or just the list
    if (fm.errorcheck(env)) {
        print("Incorrect environment specified, call env<-fm.Init(n) first.");
        return(NULL);
    }
    if (num <= 0) {
        print("Incorrect argument num");
        return(NULL);
    }
    len<-fm.fm_arraysizekadd(env,kadd);

    Val <- array(0.0, num* len);
    t<-1;
    Res <- .C("generate_fm_beliefCall",
                                          as.integer(num),
                                          as.integer(env$n),
                                          as.integer(kadd),
                                          out = as.numeric(Val) ,
                                          out1=as.integer(t),
                                          as.integer(env$m), as.integer(env$card), as.integer(env$cardpos),as.double(env$bit2card),as.double(env$card2bit),as.double(env$factorials)
                                          
                                          );
    return(Res$out);
}



fm.generate_antibuoyant <- function( env=NULL) {
# just one

    if (fm.errorcheck(env)) {
        print("Incorrect environment specified, call env<-fm.Init(n) first.");
        return(NULL);
    }

	Val <- array(0.0, env$m);
	
    Res <- .C("GenerateAntibuoyantCall",
										  as.integer(env$n),
										  
                                          out = as.numeric(Val) ,
										  as.integer(env$m), as.integer(env$card), as.integer(env$cardpos),as.double(env$bit2card),as.double(env$card2bit),as.double(env$factorials)
										  );
	return(Res$out);									  
}


 
   fm.check_monotonicity_sort_merge <- function(v, indices=NULL, env=NULL) {

	Val <- 1;
	if(fm.errorcheck(env)) {
		print("Incorrect environment specified, call env<-fm.Init(n) first.");
		return (NULL);
	}
	if(is.null(indices)) {
		indices <- array(0.0,env$m);
	}
    Res <- .C("CheckMonotonicitySortMergeCall",
									val=as.numeric(v) ,
									idx=as.numeric(indices) ,
                                          as.integer(env$m),
                                          as.integer(env$n),
                                          out = as.integer(Val) );
	return(list(out=Res$out, index=Res$idx, V=Res$val));
}
  fm.check_monotonicity_sort_insert <- function(v, indices, env=NULL) {

	Val <- 1;
	if(fm.errorcheck(env)) {
		print("Incorrect environment specified, call env<-fm.Init(n) first.");
		return (NULL);
	}

    Res <- .C("CheckMonotonicitySortInsertCall",
									val=as.numeric(v) ,
									idx=as.numeric(indices) ,
                                          as.integer(env$m),
                                          as.integer(env$n),
                                          out = as.integer(Val) );
	return(list(out=Res$out, index=Res$idx, V=Res$val));
}
 
 fm.check_monotonicity <- function(v, env=NULL) {

	Val <- 1;
	if(fm.errorcheck(env)) {
		print("Incorrect environment specified, call env<-fm.Init(n) first.");
		return (NULL);
	}
	if(env$m!=length(v)) {
		print("The environment mismatches the dimension to the fuzzy measure.");
		return (NULL);
	}
    check_monotonicity <- .C("CheckMonotonicitySimpleCall",
									as.numeric(v) ,
                                          as.integer(env$m),
                                          as.integer(env$n),
                                          out = as.integer(Val)  );
	return(as.logical(check_monotonicity$out));
}

  fm.check_monotonicity_mob <- function(v,len, env=NULL) {

	Val <- 1;
	if(fm.errorcheck(env)) {
		print("Incorrect environment specified, call env<-fm.Init(n) first.");
		return (NULL);
	}
	if(len!=length(v)) {
		print("The environment mismatches the dimension to the fuzzy measure.");
		return (NULL);
	}
    Res <- .C("CheckMonotonicityMobCall",
									as.numeric(v) ,
								    as.integer(env$n),
                                    as.integer(env$m),
									as.integer(len),
                                          out = as.integer(Val), as.integer(env$card), as.integer(env$cardpos),
										  as.double(env$bit2card),as.double(env$card2bit),as.double(env$factorials) );
	return(as.logical(Res$out));
}

  fm.check_convexity_monotonicity_mob <- function(v,len, env=NULL) {

	Val <- 1;
	if(fm.errorcheck(env)) {
		print("Incorrect environment specified, call env<-fm.Init(n) first.");
		return (NULL);
	}
	if(len!=length(v)) {
		print("The environment mismatches the dimension to the fuzzy measure.");
		return (NULL);
	}
    Res <- .C("CheckConvexityMonMobCall",
									as.numeric(v) ,
								    as.integer(env$n),
                                    as.integer(env$m),
									as.integer(len),
                                          out = as.integer(Val) , 
										  as.integer(env$card), as.integer(env$cardpos),as.double(env$bit2card),as.double(env$card2bit),
										  as.double(env$factorials));
	return(as.logical(Res$out));
}
  fm.check_monotonicity_mob_2additive <- function(v,n,temp=NULL) {

	Val <- 1;
	if(is.null(temp)) {
		temp <- array(0.0,n*n);
	}
    len<-0; #not used
    
    Res <- .C("CheckMonMob2additive2Call",
									as.numeric(v) ,
								    as.integer(n),
                                    as.integer(len),
									as.numeric(temp),
                                          out = as.integer(Val) );
	return(as.logical(Res$out));
}


fm.generate_fm_randomwalk <- function(num, n, kadd,markov, option, step,  Fn, env) {
	if(fm.errorcheck(env)) {
		print("Incorrect environment specified, call env<-fm.Init(n) first.");
		return (NULL);
	} 
    mynull<-0
    if(is.null(Fn))
    return(.Call('fm_generate_fm_randomwalkCall', PACKAGE='Rfmtool',num, n, kadd,markov, option, step,  Fn,  as.integer(env$card), as.integer(env$cardpos),as.double(env$bit2card),as.double(env$card2bit),
    as.double(env$factorials), as.integer(mynull)));
  
  mynull<-1
  return(.Call('fm_generate_fm_randomwalkCall', PACKAGE='Rfmtool',num, n, kadd,markov, option, step,  Fn,  as.integer(env$card), as.integer(env$cardpos),as.double(env$bit2card),as.double(env$card2bit),
  as.double(env$factorials), as.integer(mynull)));
}

fm.generate_fm_kinteractivedualconvex <- function(num, n, kadd,markov,  step,  Fn, env) {
	if(fm.errorcheck(env)) {
		print("Incorrect environment specified, call env<-fm.Init(n) first.");
		return (NULL);
	} 
    mynull<-0
    if(is.null(Fn))
  return(.Call('fm_generate_fm_kinteractivedualconvexCall', PACKAGE='Rfmtool', num, n, kadd, markov,  step,  Fn,  as.integer(env$card), as.integer(env$cardpos),as.double(env$bit2card),as.double(env$card2bit),
  as.double(env$factorials), as.integer(mynull)));
  
  mynull<-1
  return(.Call('fm_generate_fm_kinteractivedualconvexCall', PACKAGE='Rfmtool', num, n, kadd, markov,  step,  Fn,  as.integer(env$card), as.integer(env$cardpos),as.double(env$bit2card),as.double(env$card2bit),
  as.double(env$factorials), as.integer(mynull)));
}

fm.generate_fm_kinteractivedualconcave <- function(num, n, kadd,markov,  step,  Fn, env) {
	if(fm.errorcheck(env)) {
		print("Incorrect environment specified, call env<-fm.Init(n) first.");
		return (NULL);
	} 
    mynull<-0
    if(is.null(Fn))
  return(.Call('fm_generate_fm_kinteractivedualconcaveCall', PACKAGE='Rfmtool',num, n, kadd,markov,  step,  Fn , as.integer(env$card), as.integer(env$cardpos),as.double(env$bit2card),as.double(env$card2bit),
  as.double(env$factorials), as.integer(mynull)));
  
  mynull<-1
  return(.Call('fm_generate_fm_kinteractivedualconcaveCall', PACKAGE='Rfmtool',num, n, kadd,markov,  step,  Fn , as.integer(env$card), as.integer(env$cardpos),as.double(env$bit2card),as.double(env$card2bit),
  as.double(env$factorials), as.integer(mynull)));
}

fm.generate_fm_2additive_randomwalk2 <- function(num, n, markov,  option, step,  Fn) {
    
    mynull<-0
    if(is.null(Fn))
    return(.Call('fm_generate_fm_2additive_randomwalk2Call', PACKAGE='Rfmtool', num, n, markov, option, step,  Fn, as.integer(mynull)));
    mynull<-1
  return(.Call('fm_generate_fm_2additive_randomwalk2Call', PACKAGE='Rfmtool', num, n, markov, option, step,  Fn), as.integer(mynull));
}

fm.fitting2additive<- function(data, options=0, indexlow=(NULL), indexhigh=(NULL) , option1=0, orness=(NULL))
{
	# This function estimates the values of a k-additive standard fuzzy measure based on empirical data. 
	# The result is an array containing the values of the fuzzy measure in Mobius, ordered according to set cardinalities.
	# kadd define the complexity of fuzzy measure. if kadd is not provided, its default value is equal to the number of inputs.

	size <- dim(as.matrix(data));
	n <- size[2] - 1;
	datanum <- size[1];
	m <- n*(n-1)/2+n;
	Val <- array(0.0,m);

  	
    Value <- .C("fitting2additive", as.integer(n),
        as.integer(datanum),
        as.integer(m),
		as.integer(options), 
        as.numeric(indexlow), 
        as.numeric(indexhigh), 
        as.integer(option1), 
        as.numeric(orness),
        out = as.numeric(Val),
        as.numeric(t(data))
    );
				
	return (Value$out);
}

fm.ConvertCoMob2Kinter <- function(Mob,kadd, fullmu, env=NULL)
{
    # 
	if(fm.errorcheck(env)) {
		print("Incorrect environment specified, call env<-fm.Init(n) first.");
		return (NULL);
	}
 # length (compact or non-compact representation)
	if(fullmu==1){
		len<-env$m;
	} else {
		len<-fm.fm_arraysizekadd(env,kadd)+env$n;
	}
    Val <-  array(0.0,len);
	
    Out <- .C("ConvertCoMob2KinterCall", 
        as.integer(env$n), as.integer(kadd), as.integer(len), out = as.numeric(Val), 
        as.numeric(Mob),as.integer(fullmu),
	 as.integer(env$m), as.integer(env$card), as.integer(env$cardpos),as.double(env$bit2card),as.double(env$card2bit),
	 as.double(env$factorials));
	 
    return (Out$out);
}


fm.ChoquetCoMobKInter <- function(x, Mob, kadd,  env=NULL)
{
    # 
	if(fm.errorcheck(env)) {
		print("Incorrect environment specified, call env<-fm.Init(n) first.");
		return (NULL);
	}
 # length (compact or non-compact representation)

	len<-fm.fm_arraysizekadd(env,kadd)+env$n;
   
	ChoquetVal<-0.0;
	
    Out <- .C("ChoquetCoMobKInterCall", as.numeric(x),
        as.numeric(Mob),
        as.integer(env$n), as.integer(kadd), as.integer(len), 
        out = as.numeric(ChoquetVal), 
	 as.integer(env$m), as.integer(env$card), as.integer(env$cardpos),as.double(env$bit2card),as.double(env$card2bit),
	 as.double(env$factorials));
	 
    return (Out$out);
} 
